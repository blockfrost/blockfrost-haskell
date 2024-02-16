{-# LANGUAGE RecordWildCards #-}
-- | Cardano Utils responses

module Blockfrost.Types.Cardano.Utils
  ( DerivedAddress (..)
  , TxEval (..)
  , TxEvalValidator (..)
  , TxEvalBudget (..)
  , evalSample
  , TxEvalInput (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , FromJSONKey (..)
  , ToJSON (..)
  , ToJSONKey (..)
  , Value (Array)
  , object
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Types (FromJSONKeyFunction(..), Parser)

import Blockfrost.Types.Shared.CBOR (CBORString(..))
import Blockfrost.Types.Shared.ValidationPurpose (ValidationPurpose(..))
import Data.Text (Text)
import Data.Map (Map)
import Deriving.Aeson
import Servant.Docs (ToSample (..), singleSample)
import qualified Data.Aeson.Types
import qualified Data.Char
import qualified Data.Text
import qualified Data.Map.Strict
import qualified Text.Read

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

-- * TxEval

data TxEvalValidator = TxEvalValidator
  { _txEvalValidatorPurpose :: ValidationPurpose
  , _txEvalValidatorIndex :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON TxEvalValidator where
  toJSON = toJSON . mkOgmiosValidator

instance ToJSONKey TxEvalValidator where
  toJSONKey = Data.Aeson.Types.toJSONKeyText mkOgmiosValidator

mkOgmiosValidator
  :: TxEvalValidator
  -> Text
mkOgmiosValidator TxEvalValidator{..} =
  (    toOgmiosPurpose _txEvalValidatorPurpose
    <> ":"
    <> Data.Text.pack (show _txEvalValidatorIndex)
  )
instance FromJSON TxEvalValidator where
  parseJSON =
    withText
      "TxEvalValidator"
      parseOgmiosValidator

instance FromJSONKey TxEvalValidator where
  fromJSONKey = FromJSONKeyTextParser parseOgmiosValidator

parseOgmiosValidator
  :: Text
  -> Parser TxEvalValidator
parseOgmiosValidator =
      (\case
         [purpose, index] ->
            case fromOgmiosPurpose purpose of
              Right p ->
                case Text.Read.readMaybe (Data.Text.unpack index) of
                  Nothing -> fail $ "Expecting numeric index, got " <> (Data.Text.unpack index)
                  Just idx -> pure $ TxEvalValidator p idx
              Left e ->
                fail e
         x -> fail $ "Expecting [purpose, index], got " <> show x
      .  Data.Text.splitOn ":"
      )

toOgmiosPurpose
  :: ValidationPurpose
  -> Text
toOgmiosPurpose Spend = "spend"
toOgmiosPurpose Mint = "mint"
toOgmiosPurpose Cert = "publish"
toOgmiosPurpose Reward = "withdraw"

fromOgmiosPurpose
  :: Text
  -> Either String ValidationPurpose
fromOgmiosPurpose "spend" = Right Spend
fromOgmiosPurpose "mint" = Right Mint
fromOgmiosPurpose "publish" = Right Cert
fromOgmiosPurpose "withdraw" = Right Reward
fromOgmiosPurpose x =
  Left
    $ "Don't know how to handle Ogmios validation purpose: " 
      <> Data.Text.unpack x

validatorSample :: TxEvalValidator
validatorSample =
  TxEvalValidator
    { _txEvalValidatorPurpose = Spend
    , _txEvalValidatorIndex = 0
    }

instance ToSample TxEvalValidator where
  toSamples = pure $ singleSample validatorSample

data TxEvalBudget = TxEvalBudget
  { _txEvalBudgetMemory :: Integer -- ^ Memory budget
  , _txEvalBudgetSteps  :: Integer -- ^ CPU budget
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_txEvalBudget", CamelToSnake]] TxEvalBudget

budgetSample :: TxEvalBudget
budgetSample =
  TxEvalBudget
      { _txEvalBudgetMemory = 1765011
      , _txEvalBudgetSteps  = 503871230
      }

instance ToSample TxEvalBudget where
  toSamples = pure $ singleSample budgetSample

data TxEvalFailure = TxEvalFailure Value
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

-- | Transaction evaluation result wrapper
newtype TxEval = TxEval
  { _txEvalResult ::
      Either
        TxEvalFailure
        (Map
          TxEvalValidator
          TxEvalBudget)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TxEval where
  toJSON TxEval{..} =
    object
      [ "type" .= ("jsonwsp/response" :: Text)
      , "version" .= ("1.0" :: Text)
      , "servicename" .= ("ogmios" :: Text)
      , "methodname" .= ("EvaluateTx" :: Text)
      , "result" .= toJSON _txEvalResult
      ]

instance FromJSON TxEval where
  parseJSON = withObject "txEval" $ \o -> do
    r <- o .: "result"
    mEvalResult <- r .:? "EvaluationResult"
    case mEvalResult of
      Nothing -> TxEval . Left . TxEvalFailure <$> r .: "EvaluationFailure"
      Just evalRes -> TxEval . Right <$> parseJSON evalRes

evalSample :: TxEval
evalSample =
  TxEval
    $ Right
        (Data.Map.Strict.fromList
          [(validatorSample, budgetSample)]
        )

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
