-- | POSIX Milliseconds wrapper

module Blockfrost.Types.Shared.POSIXMillis
  ( POSIXMillis
  , POSIXTime
  , millisecondsToPosix
  , posixToMilliseconds
  , seconds
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX
import GHC.Generics
import Servant.Docs (ToSample (..), singleSample)

-- | Convert `Integer` milliseconds to `POSIXTime`
millisecondsToPosix :: Integer -> POSIXTime
millisecondsToPosix n = fromInteger n / 1000

-- | Convert `POSIXTime` to `Integer` milliseconds
posixToMilliseconds :: POSIXTime -> Integer
posixToMilliseconds t = round (t * 1000)

newtype POSIXMillis = POSIXMillis {
    unPOSIXMillis :: POSIXTime
  }
  deriving stock (Show, Eq, Generic)

seconds :: POSIXTime -> POSIXMillis
seconds = POSIXMillis

instance FromJSON POSIXMillis where
  parseJSON t = POSIXMillis . millisecondsToPosix <$> parseJSON @Integer t

instance ToJSON POSIXMillis where
  toJSON (POSIXMillis t) = toJSON $ posixToMilliseconds t
  toEncoding (POSIXMillis t) = toEncoding $ posixToMilliseconds t

instance ToSample POSIXMillis where
    toSamples _ = singleSample $ POSIXMillis $ millisecondsToPosix 1603400958947

