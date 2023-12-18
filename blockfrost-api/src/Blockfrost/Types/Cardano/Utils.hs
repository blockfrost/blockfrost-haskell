{-# LANGUAGE RecordWildCards #-}
-- | Cardano Utils responses

module Blockfrost.Types.Cardano.Utils
  ( DerivedAddress (..)
  , TxEval (..)
  , TxEvalBudget (..)
  , TxEvalResult (..)
  , evalSample
  , resultSample
  , TxEvalInput (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (Array)
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )

import Blockfrost.Types.Shared.CBOR (CBORString(..))
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), singleSample)
import qualified Data.Char

-- | Derived Shelley address
data DerivedAddress = DerivedAddress
  { _derivedAddressXpub    :: Text    -- ^ Hexadecimal xpub
  , _derivedAddressRole    :: Integer -- ^ Account role
  , _derivedAddressIndex   :: Integer -- ^ Address index
  , _derivedAddressAddress :: Text    -- ^ Derived address
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_derivedAddress", CamelToSnake]] DerivedAddress

instance ToSample DerivedAddress where
  toSamples = pure $ singleSample
    DerivedAddress
      { _derivedAddressXpub    = "d507c8f866691bd96e131334c355188b1a1d0b2fa0ab11545075aab332d77d9eb19657ad13ee581b56b0f8d744d66ca356b93d42fe176b3de007d53e9c4c4e7a"
      , _derivedAddressRole    = 0
      , _derivedAddressIndex   = 0
      , _derivedAddressAddress = "addr1q90sqnljxky88s0jsnps48jd872p7znzwym0jpzqnax6qs5nfrlkaatu28n0qzmqh7f2cpksxhpc9jefx3wrl0a2wu8q5amen7"
      }

data TxEvalBudget = TxEvalBudget
  { _txEvalBudgetMemory :: Integer -- ^ Memory budget
  , _txEvalBudgetCPU    :: Integer -- ^ CPU budget
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_txEvalBudget", CamelToSnake]] TxEvalBudget

instance ToSample TxEvalBudget where
  toSamples = pure $ singleSample
    TxEvalBudget
      { _txEvalBudgetMemory = 1700
      , _txEvalBudgetCPU    = 476468
      }

-- | Transaction evaluation result
data TxEvalResult = TxEvalResult
  { _txEvalResultValidator :: Text         -- ^ Redeemer pointer
  , _txEvalResultBudget    :: TxEvalBudget -- ^ Budget
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_txEvalResult", CamelToSnake]] TxEvalResult

resultSample :: TxEvalResult
resultSample =
  TxEvalResult
    { _txEvalResultValidator = "spend:0"
    , _txEvalResultBudget =
        TxEvalBudget
          { _txEvalBudgetMemory = 1700
          , _txEvalBudgetCPU    = 476468
          }
    }

instance ToSample TxEvalResult where
  toSamples = pure $ singleSample resultSample

-- | Transaction evaluation result wrapper
newtype TxEval = TxEval { _txEvalResult :: [TxEvalResult] }
  deriving stock (Show, Eq, Generic)

instance ToJSON TxEval where
  toJSON TxEval{..} =
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= ("evaluateTransaction" :: Text)
      , "result" .= toJSON _txEvalResult
      ]

instance FromJSON TxEval where
  parseJSON = withObject "txEval" $ \o -> do
    (mErr :: Maybe Value) <- o .:? "error"
    case mErr of
      Just err -> fail $ show err
      Nothing -> pure ()

    r <- o .: "result"
    TxEval <$> parseJSON r

evalSample :: TxEval
evalSample = TxEval (pure resultSample)

instance ToSample TxEval where
  toSamples = pure $ singleSample evalSample

data LowerLeading
instance StringModifier LowerLeading where
  getStringModifier "" = ""
  getStringModifier (c:xs) = Data.Char.toLower c : xs

-- | Transaction evaluation input for UTXO variant
data TxEvalInput = TxEvalInput
  { _txEvalInputCbor              :: CBORString  -- ^ CBOR encoded transaction
  , _txEvalInputAdditionalUtxoSet :: Value -- ^ Additional UTXO set as JSON @Value@
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_txEvalInput", LowerLeading]] TxEvalInput

instance ToSample TxEvalInput where
  toSamples = pure $ singleSample
    TxEvalInput
      { _txEvalInputCbor              = CBORString "83a40081825820daa9"
      , _txEvalInputAdditionalUtxoSet = Array mempty
      }
