-- | Types for common servics

module Blockfrost.Types.Common
  ( URLVersion (..)
  , Healthy (..)
  , ServerTime (..)
  , Metric (..)
  ) where

import Blockfrost.Types.Shared
import Data.Aeson
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples, singleSample)

-- | Root endpoint reply
data URLVersion = URLVersion
  { _urlVersionUrl     :: Text
  , _urlVersionVersion :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_urlVersion", CamelToSnake]] URLVersion

instance ToSample URLVersion where
  toSamples = pure $ singleSample $
    URLVersion "http://blockfrost.io" "0.0.0"

-- | Health endpoint reply
newtype Healthy = Healthy
  { isHealthy :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[CamelToSnake]] Healthy

instance ToSample Healthy where
  toSamples = pure $ singleSample $ Healthy False

-- | Health clock endpoint reply
newtype ServerTime = ServerTime
  { serverTime :: POSIXTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ServerTime where
  parseJSON = withObject "ServerTime"
    $ \v -> v .: "server_time"
    >>= \t -> ServerTime . millisecondsToPosix <$> parseJSON @Integer t

instance ToJSON ServerTime where
  toJSON (ServerTime t) =
    object [
      "server_time" .= posixToMilliseconds t
    ]

instance ToSample ServerTime where
    toSamples _ = singleSample $ ServerTime $ millisecondsToPosix 1603400958947

-- | Metrics response
data Metric = Metric {
    _metricTime  :: POSIXTime
  , _metricCalls :: Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_metric", CamelToSnake]] Metric

instance ToSample Metric where
    toSamples _ = samples $
      [ Metric 1612543884 42
      , Metric 1614523884 6942
      ]

-- Re-use @Metric@ for response with endpoint field
instance {-# OVERLAPS #-} ToJSON (Text, Metric) where
  toJSON (enp, m) = case toJSON m of
    (Object o) -> Object (o <> ("endpoint" .= (toJSON enp)))
    _          -> error "Absurd"

instance {-# OVERLAPS #-} FromJSON (Text, Metric) where
  parseJSON v@(Object o) = do
    enp <- o .: "endpoint"
    ticker <- parseJSON v
    return (enp, ticker)
  parseJSON _ = fail "Unexpected type for (Text, Metric)"

instance {-# OVERLAPS #-} ToSample (Text, Metric) where
  toSamples _ = samples $
    [ ("block", Metric 1612543814 182)
    , ("epoch", Metric 1612543814 42)
    , ("block", Metric 1612543812 775)
    , ("epoch", Metric 1612523884 4)
    , ("block", Metric 1612553884 89794)
    ]
