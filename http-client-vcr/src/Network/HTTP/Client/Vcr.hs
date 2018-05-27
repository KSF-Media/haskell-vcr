{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wall            #-}
module Network.HTTP.Client.Vcr where

import           Control.Concurrent.MVar      as MVar
import           Control.Monad                (void)
import           Control.Monad.Catch          (bracketOnError)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Builder      as Builder
import qualified Data.ByteString.Lazy         as LByteString
import           Data.IORef                   as IORef
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           Data.Semigroup
import qualified Data.Time                    as Time
import           Data.Vcr                     as Vcr
import qualified Network.HTTP.Client          as Http
import qualified Network.HTTP.Client.Internal as Http (Response (Response),
                                                       ResponseClose (..),
                                                       constBodyReader,
                                                       responseClose')
import           Network.HTTP.Types           as Http

toHttpResponse :: Vcr.Response -> Http.Response ByteString
toHttpResponse r = Http.Response
  { Http.responseStatus = responseStatus r
  , Http.responseVersion = fromMaybe Http.http11 $ responseHttpVersion r
  , Http.responseHeaders = responseHeaders r
  , Http.responseCookieJar = mempty
  , Http.responseBody = maybe mempty bodyString $ responseBody r
  , Http.responseClose' = Http.ResponseClose $ pure ()
  }

fromHttpResponse :: Http.Response ByteString -> Response
fromHttpResponse r = Response
  { responseStatus = Http.responseStatus r
  , responseHttpVersion = Just $ Http.responseVersion r
  , responseHeaders = Http.responseHeaders r
  , responseBody = Just Body
      { bodyEncoding = "UTF-8"
      , bodyString = Http.responseBody r
      }
  }

fromHttpRequest :: MonadIO m => Http.Request -> m Request
fromHttpRequest r = do
  bodyString <- liftIO $ getBodyString (Http.requestBody r)
  pure $ Request
    { requestMethod = Http.method r
    , requestUri = Http.getUri r
    , requestHeaders = Http.requestHeaders r
    , requestBody = Just Body
        { bodyEncoding = "UTF-8"
        , bodyString = bodyString
        }
    }
  where
    getBodyString s =
      case s of
        Http.RequestBodyLBS lbs     -> pure $ LByteString.toStrict lbs
        Http.RequestBodyBS  bs      -> pure bs
        Http.RequestBodyBuilder _size b -> pure $ LByteString.toStrict $ Builder.toLazyByteString b
        Http.RequestBodyStream _size givesPopper -> readStream givesPopper
        Http.RequestBodyStreamChunked givesPopper -> readStream givesPopper
        Http.RequestBodyIO m -> m >>= getBodyString

    readStream givesPopper = do
      -- https://github.com/snoyberg/http-client/blob/eb38c5f19f451af4e5f2596feac42fa686798c3e/http-conduit/test/main.hs#L385
      ires <- newIORef mempty
      let loop front popper = do
            bs <- popper
            if ByteString.null bs
                then writeIORef ires $ ByteString.concat $ front []
                else loop (front . (bs:)) popper
      void $ givesPopper $ loop id
      readIORef ires

matchHttpRequest :: MonadIO m => Http.Request -> m Vcr.InteractionMatcher
matchHttpRequest httpRequest = do
  request <- fromHttpRequest httpRequest
  pure $ Vcr.InteractionMatcher $ \Vcr.Interaction{..} ->
    request == interactionRequest

responseOpen
  :: (MonadIO m)
  => Recorder -> RecordMode -> InteractionMatcher
  -> (Http.Request -> IO (Http.Response Http.BodyReader))
  -> (forall a. Http.Response a -> IO ())
  -> Http.Request -> m (Http.Response Http.BodyReader)
responseOpen recorder@Recorder{..} mode@Always matcher httpResponseOpen httpResponseClose httpRequest = do
  request <- fromHttpRequest httpRequest
  replay <- Vcr.lookupResponse recorder mode matcher
  case replay of
    Just vcrResponse ->
      traverse
        (liftIO . Http.constBodyReader . LByteString.toChunks . LByteString.fromStrict)
        (toHttpResponse vcrResponse)
    Nothing ->
      liftIO
        $ bracketOnError (httpResponseOpen httpRequest) (httpResponseClose)
        $ \response -> do
            bodyRef :: IORef Builder <- newIORef mempty
            responseVar <- newEmptyMVar
            now <- Time.getCurrentTime
            atomicModifyIORef' recorderRecordingRef $ \recording -> (,()) $
              recording <> Recording (Map.singleton request (Map.singleton now responseVar))
            let finalize =
                  readIORef bodyRef >>= \bodyBuilder -> do
                    void $ tryPutMVar responseVar $ fromHttpResponse response
                      { Http.responseBody =
                          LByteString.toStrict
                            $ Builder.toLazyByteString bodyBuilder
                      }
            pure response
              { Http.responseBody = do
                  chunk <- Http.responseBody response
                  chunk <$ do
                    if ByteString.null chunk
                      then finalize
                      else modifyIORef bodyRef (<> Builder.byteString chunk)
              , Http.responseClose' = Http.ResponseClose $ do
                  Http.runResponseClose $ Http.responseClose' response
                  finalize
              }
