-- | Types for Nut.link servics

module Blockfrost.Types.NutLink
  ( NutlinkAddress (..)
  , NutlinkAddressTicker (..)
  , NutlinkTicker (..)
  ) where

import Blockfrost.Types.Shared
import Data.Aeson
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples, singleSample)

-- | Specific address metadata
data NutlinkAddress = NutlinkAddress
  { _nutlinkAddressAddress      :: Address -- ^ Bech32 encoded address
  , _nutlinkAddressMetadataUrl  :: Text -- ^ URL of the specific metadata file
  , _nutlinkAddressMetadataHash :: Text -- ^ Hash of the metadata file
  , _nutlinkAddressMetadata     :: Maybe Value -- ^ The cached metadata of the metadata_url file.
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_nutlinkAddress", CamelToSnake]] NutlinkAddress

instance ToSample NutlinkAddress where
  toSamples = pure $ singleSample $
    NutlinkAddress
      { _nutlinkAddressAddress = "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
      , _nutlinkAddressMetadataUrl = "https://nut.link/metadata.json"
      , _nutlinkAddressMetadataHash = "6bf124f217d0e5a0a8adb1dbd8540e1334280d49ab861127868339f43b3948af"
      , _nutlinkAddressMetadata = pure $ object []
      }

-- | Ticker for specific metadata oracle
data NutlinkAddressTicker = NutlinkAddressTicker
  { _nutlinkAddressTickerName        :: Text -- ^ Name of the ticker
  , _nutlinkAddressTickerCount       :: Integer -- ^ Number of ticker records
  , _nutlinkAddressTickerLatestBlock :: Integer -- ^ Block height of the latest record
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_nutlinkAddressTicker", CamelToSnake]] NutlinkAddressTicker

instance ToSample NutlinkAddressTicker where
  toSamples = pure $ samples $
    [ NutlinkAddressTicker
        { _nutlinkAddressTickerName = "ADAUSD"
        , _nutlinkAddressTickerCount = 1980038
        , _nutlinkAddressTickerLatestBlock = 2657092
        }
    , NutlinkAddressTicker
        { _nutlinkAddressTickerName = "ADAEUR"
        , _nutlinkAddressTickerCount = 1980038
        , _nutlinkAddressTickerLatestBlock = 2657092
        }
    , NutlinkAddressTicker
        { _nutlinkAddressTickerName = "ADABTC"
        , _nutlinkAddressTickerCount = 1980038
        , _nutlinkAddressTickerLatestBlock = 2657092
        }
    ]

-- | Specific ticker record
data NutlinkTicker = NutlinkTicker
  { _nutlinkTickerTxHash      :: TxHash -- ^ Hash of the transaction
  , _nutlinkTickerBlockHeight :: Integer -- ^ Block height of the record
  , _nutlinkTickerTxIndex     :: Integer -- ^ Transaction index within the block
  , _nutlinkTickerPayload     :: Value -- ^ Content of the ticker
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_nutlinkTicker", CamelToSnake]] NutlinkTicker

instance ToSample NutlinkTicker where
  toSamples = pure $ singleSample $
    NutlinkTicker
      { _nutlinkTickerTxHash = "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b"
      , _nutlinkTickerBlockHeight = 2657092
      , _nutlinkTickerTxIndex = 8
      , _nutlinkTickerPayload = object []
      }

-- Re-use @NutlinkTicker@ for response with address field
instance {-# OVERLAPS #-} ToJSON (Address, NutlinkTicker) where
  toJSON (addr, nt) = case toJSON nt of
    (Object o) -> Object (o <> ("address" .= (toJSON addr)))
    _          -> error "Absurd"

instance {-# OVERLAPS #-} FromJSON (Address, NutlinkTicker) where
  parseJSON v@(Object o) = do
    addr <- o .: "address"
    ticker <- parseJSON v
    return (addr, ticker)
  parseJSON _ = fail "Unexpected type for (Address, NutlinkTicker)"
