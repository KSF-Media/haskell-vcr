{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Vcr where

import           Control.Applicative
import           Data.Aeson           (FromJSON (parseJSON), ToJSON (toJSON),
                                       (.:), (.:?), (.=))
import qualified Data.Aeson           as Json
import qualified Data.Aeson.Types     as Json (Parser)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as ByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import           Data.Foldable        (foldl)
import qualified Data.HashMap.Lazy    as HashMap
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as Text
import qualified Data.Time            as Time
import           Data.Traversable     (for)
import           Data.Typeable        (Typeable)
import qualified Data.Vector          as Vector
import qualified Data.Yaml            as Yaml
import           GHC.Generics         (Generic)
import           Network.HTTP.Types   as Http

data Casette = Casette
  { casetteHttpInteractions :: [Interaction]
  , casetteRecordedWith     :: Maybe Text
  } deriving (Show, Typeable, Generic)

instance ToJSON Casette where
  toJSON Casette{..} =
    Json.object
      [ "http_interactions" .= casetteHttpInteractions
      , "recorded_with"     .= casetteRecordedWith
      ]

instance FromJSON Casette where
  parseJSON = Json.withObject "Casette" $ \o -> do
    casetteHttpInteractions <-
      maybe (o .: "http_interactions") pure =<< o .:? "interactions"
    casetteRecordedWith <- o .:? "recorded_with"
    pure Casette{..}

data Interaction = Interaction
  { interactionRequest    :: Request
  , interactionResponse   :: Response
  , interactionRecordedAt :: Maybe Time.ZonedTime
  } deriving (Show, Typeable, Generic)

timestampFormat :: String
timestampFormat = "%a, %d %b %Y %H:%M:%S %Z"

instance ToJSON Interaction where
  toJSON Interaction{..} =
    Json.object
      [ "request"     .= interactionRequest
      , "response"    .= interactionResponse
      , "recorded_at" .= (formatTimestamp <$> interactionRecordedAt)
      ]
    where
      formatTimestamp = Time.formatTime Time.defaultTimeLocale timestampFormat

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
        Time.parseTimeM True Time.defaultTimeLocale timestampFormat

data Request = Request
  { requestMethod  :: Http.Method
  , requestUri     :: Text
  , requestHeaders :: [Http.Header]
  , requestBody    :: Maybe Body
  } deriving (Show, Eq, Ord, Typeable, Generic)

instance ToJSON Request where
  toJSON Request{..} =
    Json.object
      [ "method"  .= Text.decodeUtf8 requestMethod
      , "uri"     .= requestUri
      , "headers" .= headersToJson requestHeaders
      , "body"    .= requestBody
      ]

instance FromJSON Request where
  parseJSON = Json.withObject "Request" $ \o -> do
    requestMethod <- Text.encodeUtf8 <$> o .: "method"
    requestUri    <- o .: "uri"
    requestBody   <- o .: "body"
    requestHeaders <-
      maybe (pure []) parseJsonHeaders =<< o .:? "headers"
    pure Request{..}

data Response = Response
  { responseStatus      :: Http.Status
  , responseHeaders     :: [Http.Header]
  , responseBody        :: Maybe Body
  , responseHttpVersion :: Maybe Text -- TODO: Use Http.HttpVersion
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
      , "http_version" .= responseHttpVersion
      ]

instance FromJSON Response where
  parseJSON = Json.withObject "Response" $ \o -> do
    responseStatus <-
      parseStatus =<< o .: "status"
    responseHeaders <-
      maybe (pure []) parseJsonHeaders =<< o .:? "headers"
    responseBody <- o .:? "body"
    responseHttpVersion <- o .:? "http_version"
    pure Response{..}
    where
      parseStatus = Json.withObject "Status" $ \o -> do
        statusCode <- o .: "code"
        statusMessage <- Text.encodeUtf8 <$> o .: "message"
        pure Http.Status{..}

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

instance ToJSON Body where
  toJSON Body{..} =
    Json.object
      [ "encoding" .= bodyEncoding
      , "string" .=
          case bodyEncoding of
            -- | TODO: support more fun encodings
            --   base64?, gzip-base64?
            "UTF-8" -> Text.decodeUtf8 bodyString
            _       -> Text.decodeUtf8 bodyString
      ]

instance FromJSON Body where
  parseJSON = Json.withObject "Body" $ \o -> do
    bodyEncoding <- fromMaybe "UTF-8" <$> o .:? "encoding"
    let decode =
          case bodyEncoding of
            "UTF-8" -> Text.encodeUtf8
            -- | TODO: support more fun encodings
            --   base64?, gzip-base64?
            _       -> fail "Unsupported body encoding"
    bodyString  <- decode <$> o .: "string"
    pure Body{..}


