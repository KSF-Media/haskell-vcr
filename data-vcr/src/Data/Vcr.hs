{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
module Data.Vcr where

import           Control.Applicative
import           Control.Concurrent.MVar as MVar
import           Control.Monad.Catch     (MonadMask, MonadThrow, bracket,
                                          throwM)

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON (parseJSON), ToJSON (toJSON),
                                          (.:), (.:?), (.=))
import qualified Data.Aeson              as Json
import qualified Data.Aeson.Types        as Json (Parser)
import           Data.ByteString         (ByteString)
import qualified Data.CaseInsensitive    as CaseInsensitive
import           Data.Foldable           (fold)
import qualified Data.HashMap.Lazy       as HashMap
import           Data.IORef              as IORef
import           Data.List               (find)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Time               as Time
import           Data.Traversable        (for)
import           Data.Typeable           (Typeable)
import qualified Data.Vector             as Vector
import qualified Data.Yaml               as Yaml
import           GHC.Generics            (Generic)
import           Network.HTTP.Types      as Http
import qualified Network.URI             as Network
import qualified System.Directory        as Directory
import qualified System.FilePath         as FilePath
import           Text.Read               (readMaybe)


decodeCassetteFile :: (MonadIO m, MonadThrow m) => FilePath -> m Cassette
decodeCassetteFile path =
  either throwM pure =<< liftIO (Yaml.decodeFileEither path)

encodeCassetteFile :: (MonadIO m) => FilePath -> Cassette -> m ()
encodeCassetteFile path cassette =
  liftIO $ Yaml.encodeFile path cassette

data Cassette = Cassette
  { cassetteHttpInteractions :: [Interaction]
  , cassetteRecordedWith     :: Maybe Text
  } deriving (Show, Typeable, Generic)

instance Semigroup Cassette where
  c1 <> c2 = Cassette
    { cassetteHttpInteractions =
        cassetteHttpInteractions c1 <> cassetteHttpInteractions c2
    , cassetteRecordedWith =
        cassetteRecordedWith c2 <|> cassetteRecordedWith c1
    }

instance Monoid Cassette where
  mempty = Cassette
    { cassetteHttpInteractions = mempty
    , cassetteRecordedWith = mempty
    }
  mappend = (<>)

instance ToJSON Cassette where
  toJSON Cassette{..} =
    Json.object
      [ "http_interactions" .= cassetteHttpInteractions
      , "recorded_with"     .= cassetteRecordedWith
      ]

instance FromJSON Cassette where
  parseJSON = Json.withObject "Cassette" $ \o -> do
    cassetteHttpInteractions <-
      maybe (o .: "http_interactions") pure =<< o .:? "interactions"
    cassetteRecordedWith <- o .:? "recorded_with"
    pure Cassette{..}

data Interaction = Interaction
  { interactionRequest    :: Request
  , interactionResponse   :: Response
  , interactionRecordedAt :: Maybe Time.ZonedTime
  } deriving (Show, Typeable, Generic)

timestampFormat :: String
timestampFormat = "%a, %d %b %Y %H:%M:%S %Z"

extendedTimeLocale :: Time.TimeLocale
extendedTimeLocale = Time.defaultTimeLocale
  { Time.knownTimeZones = [ Time.TimeZone (2 * 60) False "EET"
                          , Time.TimeZone (3 * 60) False "EEST"
                          ] <> Time.knownTimeZones Time.defaultTimeLocale
  }

instance ToJSON Interaction where
  toJSON Interaction{..} =
    Json.object
      [ "request"     .= interactionRequest
      , "response"    .= interactionResponse
      , "recorded_at" .= (formatTimestamp <$> interactionRecordedAt)
      ]
    where
      formatTimestamp = Time.formatTime extendedTimeLocale timestampFormat

instance FromJSON Interaction where
  parseJSON = Json.withObject "Interaction" $ \o -> do
    interactionRequest    <- o .: "request"
    interactionResponse   <- o .: "response"
    interactionRecordedAt <- do
      recordedAt <- o .:? "recorded_at"
      for recordedAt $ \timestamp ->
        case parseTimestamp timestamp of
          Nothing        -> fail $ "Failed to parseTime according to RFC822"
          Just zonedTime -> pure zonedTime
    pure Interaction{..}
    where
      parseTimestamp =
        Time.parseTimeM True extendedTimeLocale timestampFormat

data Request = Request
  { requestMethod  :: Http.Method
  , requestUri     :: Network.URI
  , requestHeaders :: [Http.Header]
  , requestBody    :: Maybe Body
  } deriving (Show, Eq, Ord, Typeable, Generic)

instance ToJSON Request where
  toJSON Request{..} =
    Json.object
      [ "method"  .= Text.decodeUtf8 requestMethod
      , "uri"     .= Network.uriToString id requestUri ""
      , "headers" .= headersToJson requestHeaders
      , "body"    .= requestBody
      ]

instance FromJSON Request where
  parseJSON = Json.withObject "Request" $ \o -> do
    requestMethod <- Text.encodeUtf8 <$> o .: "method"
    requestUri    <- parseUri =<< o .: "uri"
    requestBody   <- o .: "body"
    requestHeaders <-
      maybe (pure []) parseJsonHeaders =<< o .:? "headers"
    pure Request{..}
    where
      parseUri (Network.parseURI -> Just uri) = pure uri
      parseUri uri = fail $ "Failed to parse uri: " <> uri

data Response = Response
  { responseStatus      :: Http.Status
  , responseHeaders     :: [Http.Header]
  , responseBody        :: Maybe Body
  , responseHttpVersion :: Maybe Http.HttpVersion
  } deriving (Show, Eq, Ord, Typeable, Generic)

instance ToJSON Response where
  toJSON Response{..} =
    Json.object
      [ "status"       .= Json.object
          [ "code"    .= Http.statusCode responseStatus
          , "message" .= Text.decodeUtf8 (Http.statusMessage responseStatus)
          ]
      , "headers"      .= headersToJson responseHeaders
      , "body"         .= responseBody
      , "http_version" .=
          fmap
            (\Http.HttpVersion{..} -> show httpMajor <> "." <> show httpMinor)
            responseHttpVersion
      ]
instance FromJSON Response where
  parseJSON = Json.withObject "Response" $ \o -> do
    responseStatus <-
      parseStatus =<< o .: "status"
    responseHeaders <-
      maybe (pure []) parseJsonHeaders =<< o .:? "headers"
    responseBody <- o .:? "body"
    responseHttpVersion <- traverse parseVersion =<< o .:? "http_version"
    pure Response{..}
    where
      parseStatus = Json.withObject "Status" $ \o -> do
        statusCode <- o .: "code"
        statusMessage <- Text.encodeUtf8 <$> o .: "message"
        pure Http.Status{..}

      parseVersion v
        | [major, minor] <- Text.splitOn "." v
        , Just httpMajor <- readMaybe $ Text.unpack major
        , Just httpMinor <- readMaybe $ Text.unpack minor
        = pure Http.HttpVersion{..}
        | otherwise = fail $ "Failed to parse http version: " <> Text.unpack v

parseJsonHeaders :: Json.Value -> Json.Parser [Http.Header]
parseJsonHeaders =
  Json.withObject "Headers" $
    fmap concat . mapM (uncurry parseHeader) . HashMap.toList
  where
    parseHeader key =
      Json.withArray "Header.values" $ \values ->
        for (Vector.toList values) $ Json.withText "text" $ \value ->
          pure (CaseInsensitive.mk $ Text.encodeUtf8 key, Text.encodeUtf8 value)

headersToJson :: [Http.Header] -> Json.Value
headersToJson =
    Json.Object . fmap toJSON . foldl addHeader HashMap.empty
  where
    addHeader headers (k, v) =
      HashMap.insertWith
        (\new old -> old <> new)
        (Text.decodeUtf8 $ CaseInsensitive.original k)
        ([Text.decodeUtf8 v])
        headers

data Body = Body
  { bodyEncoding :: Text
  , bodyString   :: ByteString
  } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Construct a 'Body' from bytestring in UTF-8 encoding.
bodyUtf8 :: ByteString -> Body
bodyUtf8 body = Body
  { bodyEncoding = "UTF-8"
  , bodyString = body
  }

-- TODO: Support more funny encodings ASCII, Latin1, KOI-8, base64?, gzip-base64?

instance ToJSON Body where
  toJSON Body{..} =
    Json.object
      [ "encoding" .= bodyEncoding
      , "string" .=
          case bodyEncoding of
            "UTF-8" -> Text.decodeUtf8 bodyString
            _       -> Text.decodeUtf8 bodyString
      ]

instance FromJSON Body where
  parseJSON = Json.withObject "Body" $ \o -> do
    bodyEncoding <- fromMaybe "UTF-8" <$> o .:? "encoding"
    bodyString <- do
      string <- o .: "string"
      case bodyEncoding of
        "UTF-8" -> pure $ Text.encodeUtf8 string
        _       -> fail "Unsupported body encoding"
    pure Body{..}

newtype LoadedCassettes = LoadedCassettes [(FilePath, Cassette)]
  deriving (Show, Semigroup, Monoid)

loadedInteractions :: LoadedCassettes -> [(FilePath, Interaction)]
loadedInteractions (LoadedCassettes cassettes) =
  cassettes >>= \(path, cassette) ->
    (path,) <$> cassetteHttpInteractions cassette

loadCassette
  :: (MonadIO m, MonadThrow m) => FilePath -> LoadedCassettes -> m LoadedCassettes
loadCassette path (LoadedCassettes cassettes) = do
  fileExists <- liftIO $ Directory.doesFileExist path
  cassette <- if fileExists then decodeCassetteFile path else pure mempty
  pure $ LoadedCassettes $ (path, cassette) : cassettes

newtype InteractionMatcher = InteractionMatcher (Interaction -> Bool)

matchRequest :: Request -> InteractionMatcher
matchRequest request =
  InteractionMatcher $ \Interaction{..} ->
    request == interactionRequest

findInteraction
  :: InteractionMatcher -> LoadedCassettes
  -> Maybe (FilePath, Interaction)
findInteraction (InteractionMatcher matcher) =
  find (matcher . snd) . loadedInteractions

data RecordMode
  = -- ^ - always replay previously recorded interactions
    --   - always record new interactions
    Always
--  TODO: Once
--  TODO: NewEpisodes
--  TODO: None
--  TODO: All
  deriving (Show, Eq, Ord, Enum)

newtype Recording = Recording (Map Request (Map Time.UTCTime (MVar Response)))
  deriving (Semigroup, Monoid)

cassetteRecording :: (MonadIO m) => Cassette -> m Recording
cassetteRecording Cassette{..} = liftIO $ do
  now <- Time.getCurrentTime
  fmap fold $ for cassetteHttpInteractions $ \Interaction{..} -> do
    Recording
      . Map.singleton interactionRequest
      . Map.singleton (maybe now Time.zonedTimeToUTC interactionRecordedAt)
      <$> newMVar interactionResponse

recordingInteractions
  :: MonadIO m => Recording -> m [Interaction]
recordingInteractions (Recording recording) = liftIO $ fmap (concat . concat) $ do
  for (Map.toList recording) $ \(interactionRequest, responses) ->
    for (Map.toList responses) $ \(recordedAt, responseVar) -> do
      interactionRecordedAt <- Just <$> Time.utcToLocalZonedTime recordedAt
      interactionResponse <- takeMVar responseVar
      pure [ Interaction{..} ]

recordingCassette :: MonadIO m => Recording -> m Cassette
recordingCassette recording = do
  cassetteHttpInteractions <- recordingInteractions recording
  cassetteRecordedWith <- pure $ Just "haskell/data-vcr"
  pure Cassette{..}

data Recorder = Recorder
  { recorderRecordingRef    :: IORef Recording
  , recorderLoadedCassettes :: LoadedCassettes
  }

loadRecorderCassette :: (MonadIO m, MonadThrow m) => FilePath -> Recorder -> m Recorder
loadRecorderCassette path recorder = do
  loadedCassettes <- loadCassette path $ recorderLoadedCassettes recorder
  pure recorder
    { recorderLoadedCassettes = loadedCassettes }

loadRecorder :: (MonadIO m, MonadThrow m) => FilePath -> Recorder -> m Recorder
loadRecorder path recorder = liftIO $ do
  loaded@(LoadedCassettes [(_path, cassette)]) <- loadCassette path mempty
  recording <- newIORef =<< cassetteRecording cassette
  pure recorder
    { recorderRecordingRef = recording
    , recorderLoadedCassettes = loaded <> recorderLoadedCassettes recorder
    }

createRecorder :: MonadIO m => m Recorder
createRecorder = do
  let recorderLoadedCassettes = mempty
  recorderRecordingRef <- liftIO $ newIORef $ mempty
  pure Recorder{..}

saveRecorder :: MonadIO m => FilePath -> Recorder -> m ()
saveRecorder path Recorder{..} = do
  recording <- liftIO $ readIORef recorderRecordingRef
  cassette <- recordingCassette recording
  liftIO $ Directory.createDirectoryIfMissing True $ FilePath.takeDirectory path
  encodeCassetteFile path cassette

withRecorder
  :: (MonadIO m, MonadMask m)
  => FilePath -> (Recorder -> m a) -> m a
withRecorder path =
  bracket (createRecorder >>= loadRecorder path) (saveRecorder path)

-- | Lookup response that can be replayed for a given request.
--   Acts according to specified mode and interaction matcher.
lookupResponse :: MonadIO m => Recorder -> RecordMode -> InteractionMatcher -> m (Maybe Response)
lookupResponse Recorder{..} Always matcher =
  -- Currently we don't actually need any IO here, but we may want to have it in the future
  -- to access IORefs, or execute user provided functions that allow IO.
  -- Thus the type already requires MonadIO.
  pure $ do
    (_cassettePath, Interaction{..}) <- findInteraction matcher recorderLoadedCassettes
    pure interactionResponse

recordResponse :: MonadIO m => Recorder -> Time.UTCTime -> Request -> Response -> m ()
recordResponse Recorder{..} time request response = liftIO $ do
  responseVar <- newMVar response
  IORef.atomicModifyIORef' recorderRecordingRef $ \recording -> (,()) $
    recording <> Recording (Map.singleton request (Map.singleton time responseVar))
