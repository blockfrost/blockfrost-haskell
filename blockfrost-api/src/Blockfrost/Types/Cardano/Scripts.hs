-- | Cardano Scripts responses

module Blockfrost.Types.Cardano.Scripts
  ( Script (..)
  , ScriptType (..)
  , ScriptRedeemer (..)
  , ScriptDatum (..)
  , ScriptJSON (..)
  , ScriptCBOR (..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples, singleSample)

import Blockfrost.Types.Shared
import Blockfrost.Types.Cardano.Transactions (ValidationPurpose(Spend))

-- | Script type
data ScriptType = Plutus | Timelock
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ConstructorTagModifier '[ToLower]] ScriptType

instance ToSample ScriptType where
  toSamples = pure $ samples [ Plutus, Timelock ]

-- | Script info
data Script = Script
  { _scriptScriptHash     :: ScriptHash -- ^ Hash of the script
  , _scriptType           :: ScriptType -- ^ Type of the script language
  , _scriptSerialisedSize :: Maybe Integer -- ^ The size of the CBOR serialised script, if a Plutus script
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_script", CamelToSnake]] Script

instance ToSample Script where
  toSamples = pure $ singleSample
    Script
      { _scriptScriptHash = "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
      , _scriptType = Plutus
      , _scriptSerialisedSize = Just 3119
      }

-- | Script redeemer
data ScriptRedeemer = ScriptRedeemer
  { _scriptRedeemerTxHash    :: TxHash -- ^ Hash of the transaction
  , _scriptRedeemerTxIndex   :: Integer -- ^ Index of the redeemer within a transaction
  , _scriptRedeemerPurpose   :: ValidationPurpose -- ^ Validation purpose
  , _scriptRedeemerDatumHash :: Text -- ^ Datum hash
  , _scriptRedeemerUnitMem   :: Quantity -- ^ The budget in Memory to run a script
  , _scriptRedeemerUnitSteps :: Quantity -- ^ The budget in Steps to run a script
  , _scriptRedeemerFee       :: Lovelaces -- ^ The fee consumed to run the script
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_scriptRedeemer", CamelToSnake]] ScriptRedeemer

instance ToSample ScriptRedeemer where
  toSamples = pure $ singleSample
    ScriptRedeemer
      { _scriptRedeemerTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0"
      , _scriptRedeemerTxIndex = 0
      , _scriptRedeemerPurpose = Spend
      , _scriptRedeemerDatumHash = "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
      , _scriptRedeemerUnitMem = 1700
      , _scriptRedeemerUnitSteps = 476468
      , _scriptRedeemerFee = 172033
      }

newtype ScriptDatum = ScriptDatum { _scriptDatumJsonValue :: Value }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_scriptDatum", CamelToSnake]] ScriptDatum

newtype ScriptJSON = ScriptJSON { _scriptJsonJson :: Maybe Value }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_scriptJson", CamelToSnake]] ScriptJSON

newtype ScriptCBOR = ScriptCBOR { _scriptCborCbor :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_scriptCbor", CamelToSnake]] ScriptCBOR
