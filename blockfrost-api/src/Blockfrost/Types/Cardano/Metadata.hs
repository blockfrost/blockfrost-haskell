-- | Transaction metadata

module Blockfrost.Types.Cardano.Metadata
  ( TxMeta (..)
  , TxMetaJSON (..)
  , TxMetaCBOR (..)
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples)

import Blockfrost.Types.Shared

-- | Transaction metadata label in use
data TxMeta = TxMeta
  { _txMetaLabel :: Text -- ^ Metadata label
  , _txMetaCip10 :: Maybe Text -- ^ CIP10 defined description
  , _txMetaCount :: Quantity -- ^ The count of metadata entries with a specific label
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_txMeta", CamelToSnake]] TxMeta

instance ToSample TxMeta where
  toSamples = pure $ samples
    [ TxMeta "1990" Nothing 1
    , TxMeta "1967" (Just "nut.link metadata oracles registry") 3
    , TxMeta "1968" (Just "nut.link metadata oracles data points") 16321
    ]

-- | Transaction metadata content in JSON
data TxMetaJSON = TxMetaJSON
  { _txMetaJSONTxHash       :: TxHash -- ^ Transaction hash that contains the specific metadata
  , _txMetaJSONJSONMetadata :: Maybe Value -- ^ Content of the JSON metadata
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_txMetaJSON", CamelToSnake]] TxMetaJSON

instance ToSample TxMetaJSON where
  toSamples =
    let oracleMeta val =
          object [
            "ADAUSD" .=
              [ object [ "value" .= (val :: Text)
                       , "source" .= ("ergoOracles" :: Text) ]
              ]
          ]
    in pure $ samples
    [ TxMetaJSON
        "257d75c8ddb0434e9b63e29ebb6241add2b835a307aa33aedba2effe09ed4ec8"
        (Just $ oracleMeta "0.10409800535729975")
    , TxMetaJSON
        "e865f2cc01ca7381cf98dcdc4de07a5e8674b8ea16e6a18e3ed60c186fde2b9c"
        (Just $ oracleMeta "0.15409850555139935")
    , TxMetaJSON
        "4237501da3cfdd53ade91e8911e764bd0699d88fd43b12f44a1f459b89bc91be"
        Nothing
    ]

-- | Transaction metadata content in CBOR
data TxMetaCBOR = TxMetaCBOR
  { _txMetaCBORTxHash   :: TxHash -- ^ Transaction hash that contains the specific metadata
  , _txMetaCBORMetadata :: Maybe Text -- ^ Content of the CBOR metadata
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_txMetaCBOR", CamelToSnake]] TxMetaCBOR

instance ToSample TxMetaCBOR where
  toSamples = pure $ samples
    [ TxMetaCBOR
        "257d75c8ddb0434e9b63e29ebb6241add2b835a307aa33aedba2effe09ed4ec8"
        Nothing
    , TxMetaCBOR
        "e865f2cc01ca7381cf98dcdc4de07a5e8674b8ea16e6a18e3ed60c186fde2b9c"
        Nothing
    , TxMetaCBOR
        "4237501da3cfdd53ade91e8911e764bd0699d88fd43b12f44a1f459b89bc91be"
        (Just "a100a16b436f6d62696e6174696f6e8601010101010c")
    ]
