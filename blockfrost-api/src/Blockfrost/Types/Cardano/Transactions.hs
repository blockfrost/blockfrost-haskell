-- | Cardano Transactions responses

module Blockfrost.Types.Cardano.Transactions
  ( Transaction (..)
  , TransactionUtxos (..)
  , UtxoInput (..)
  , UtxoOutput (..)
  , TransactionRedeemer (..)
  , TransactionStake (..)
  , TransactionDelegation (..)
  , TransactionWithdrawal (..)
  , Pot (..)
  , TransactionMir (..)
  , TransactionPoolUpdate (..)
  , PoolUpdateMetadata (..)
  , TransactionPoolRetiring (..)
  , TransactionMetaJSON (..)
  , TransactionMetaCBOR (..)
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Deriving.Aeson
import qualified Money
import Servant.Docs (ToSample (..), samples, singleSample)

import Blockfrost.Types.Cardano.Pools
import Blockfrost.Types.Cardano.Scripts (InlineDatum (..), ScriptDatumCBOR (..))
import Blockfrost.Types.Shared

-- | Information about a transaction
data Transaction = Transaction
  { _transactionHash                 :: Text -- ^ Transaction hash
  , _transactionBlock                :: BlockHash -- ^ Block hash
  , _transactionBlockHeight          :: Integer -- ^ Block number
  , _transactionSlot                 :: Slot -- ^ Slot number
  , _transactionIndex                :: Integer -- ^ Transaction index within the block
  , _transactionOutputAmount         :: [Amount] -- ^ Transaction outputs
  , _transactionFees                 :: Lovelaces -- ^ Fees of the transaction in Lovelaces
  , _transactionDeposit              :: Lovelaces -- ^ Deposit within the transaction in Lovelaces
  , _transactionSize                 :: Integer -- ^ Size of the transaction in Bytes
  , _transactionInvalidBefore        :: Maybe Text -- ^ Left (included) endpoint of the timelock validity intervals
  , _transactionInvalidHereafter     :: Maybe Text -- ^ Right (excluded) endpoint of the timelock validity intervals
  , _transactionUtxoCount            :: Integer -- ^ Count of UTXOs within the transaction
  , _transactionWithdrawalCount      :: Integer -- ^ Count of the withdrawals within the transaction
  , _transactionMirCertCount         :: Integer -- ^  Count of the MIR certificates within the transaction
  , _transactionDelegationCount      :: Integer -- ^ Count of the delegations within the transaction
  , _transactionStakeCertCount       :: Integer -- ^ Count of the stake keys (de)registration and delegation certificates within the transaction
  , _transactionPoolUpdateCount      :: Integer -- ^ Count of the stake pool registration and update certificates within the transaction
  , _transactionPoolRetireCount      :: Integer -- ^ Count of the stake pool retirement certificates within the transaction
  , _transactionAssetMintOrBurnCount :: Integer -- ^ Count of asset mints and burns within the transaction
  , _transactionRedeemerCount        :: Integer -- ^ Count of redeemers within the transaction
  , _transactionValidContract        :: Bool    -- ^ True if this is a valid transaction, False in case of contract validation failure
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transaction", CamelToSnake]] Transaction

instance ToSample Transaction where
  toSamples = pure $ singleSample
    Transaction
      { _transactionHash = "1e043f100dce12d107f679685acd2fc0610e10f72a92d412794c9773d11d8477"
      , _transactionBlock = "356b7d7dbb696ccd12775c016941057a9dc70898d87a63fc752271bb46856940"
      , _transactionBlockHeight = 123456
      , _transactionSlot = 42000000
      , _transactionIndex = 1
      , _transactionOutputAmount = sampleAmounts
      , _transactionFees = 182485
      , _transactionDeposit = 0
      , _transactionSize = 433
      , _transactionInvalidBefore = Nothing
      , _transactionInvalidHereafter = Just "13885913"
      , _transactionUtxoCount = 4
      , _transactionWithdrawalCount = 0
      , _transactionMirCertCount =  0
      , _transactionDelegationCount = 0
      , _transactionStakeCertCount = 0
      , _transactionPoolUpdateCount = 0
      , _transactionPoolRetireCount = 0
      , _transactionAssetMintOrBurnCount = 0
      , _transactionRedeemerCount = 0
      , _transactionValidContract = True
      }

-- | Transaction input UTxO
data UtxoInput = UtxoInput
  { _utxoInputAddress     :: Address -- ^ Input address
  , _utxoInputAmount      :: [Amount]
  , _utxoInputTxHash      :: TxHash -- ^ Hash of the UTXO transaction
  , _utxoInputOutputIndex :: Integer -- ^ UTXO index in the transaction
  , _utxoInputCollateral  :: Bool -- ^ UTXO is a script collateral input
  , _utxoInputDataHash    :: Maybe DatumHash -- ^ The hash of the transaction output datum
  , _utxoInputInlineDatum :: Maybe InlineDatum -- ^ CBOR encoded inline datum
  , _utxoInputReferenceScriptHash :: Maybe ScriptHash -- ^ The hash of the reference script of the input
  , _utxoInputReference   :: Bool -- ^ Whether the input is a reference transaction input
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoInput", CamelToSnake]] UtxoInput

instance ToSample UtxoInput where
  toSamples = pure $ singleSample utxoInSample

utxoInSample :: UtxoInput
utxoInSample =
  UtxoInput
    { _utxoInputAddress = "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv"
    , _utxoInputAmount = sampleAmounts
    , _utxoInputTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0"
    , _utxoInputOutputIndex = 0
    , _utxoInputCollateral = False
    , _utxoInputDataHash = Just "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    , _utxoInputInlineDatum = Nothing
    , _utxoInputReferenceScriptHash = Just "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1"
    , _utxoInputReference = False
    }

-- | Transaction output UTxO
data UtxoOutput = UtxoOutput
  { _utxoOutputAddress     :: Address -- ^ Output address
  , _utxoOutputAmount      :: [Amount] -- ^ Transaction output amounts
  , _utxoOutputDataHash    :: Maybe DatumHash -- ^ The hash of the transaction output datum
  , _utxoOutputOutputIndex :: Integer -- ^ UTXO index in the transaction
  , _utxoOutputCollateral  :: Bool -- ^ UTXO is a script collateral output
  , _utxoOutputInlineDatum :: Maybe InlineDatum -- ^ CBOR encoded inline datum
  , _utxoOutputReferenceScriptHash :: Maybe ScriptHash -- ^ The hash of the reference script of the output
  } deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoOutput", CamelToSnake]] UtxoOutput

instance ToSample UtxoOutput where
  toSamples = pure $ singleSample utxoOutSample

utxoOutSample :: UtxoOutput
utxoOutSample =
  UtxoOutput
    { _utxoOutputAddress = "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv"
    , _utxoOutputAmount = sampleAmounts
    , _utxoOutputDataHash = Just "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    , _utxoOutputOutputIndex = 0
    , _utxoOutputCollateral = False
    , _utxoOutputInlineDatum = Just $ InlineDatum $ ScriptDatumCBOR "19a6aa"
    , _utxoOutputReferenceScriptHash = Just "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1"
    }

-- | Transaction UTxOs
data TransactionUtxos = TransactionUtxos
  { _transactionUtxosHash    :: TxHash -- ^ Transaction hash
  , _transactionUtxosInputs  :: [UtxoInput] -- ^ Transaction inputs
  , _transactionUtxosOutputs :: [UtxoOutput] -- ^ Transaction outputs
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionUtxos", CamelToSnake]] TransactionUtxos

instance ToSample TransactionUtxos where
  toSamples = pure $ singleSample
    TransactionUtxos
      { _transactionUtxosHash = "1e043f100dce12d107f679685acd2fc0610e10f72a92d412794c9773d11d8477"
      , _transactionUtxosInputs = pure utxoInSample
      , _transactionUtxosOutputs = pure utxoOutSample
      }

sampleAmounts :: [Amount]
sampleAmounts =
  [ AdaAmount 42000000
  , AssetAmount
      $ Money.mkSomeDiscrete
          "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
          unitScale
          12
  ]

-- | Transaction redeemer
data TransactionRedeemer = TransactionRedeemer
  { _transactionRedeemerTxIndex   :: Integer -- ^ Index of the redeemer within a transaction
  , _transactionRedeemerPurpose   :: ValidationPurpose -- ^ Validation purpose
  , _transactionRedeemerScriptHash:: ScriptHash -- ^ Script hash
  , _transactionRedeemerRedeemerDataHash  :: DatumHash -- ^ Redeemer data hash
  , _transactionRedeemerDatumHash :: DatumHash -- ^ Datum hash (DEPRECATED)
  , _transactionRedeemerUnitMem   :: Quantity -- ^ The budget in Memory to run a script
  , _transactionRedeemerUnitSteps :: Quantity -- ^ The budget in Steps to run a script
  , _transactionRedeemerFee       :: Lovelaces -- ^ The fee consumed to run the script
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionRedeemer", CamelToSnake]] TransactionRedeemer

instance ToSample TransactionRedeemer where
  toSamples = pure $ singleSample
    TransactionRedeemer
      { _transactionRedeemerTxIndex = 0
      , _transactionRedeemerPurpose = Spend
      , _transactionRedeemerScriptHash = "ec26b89af41bef0f7585353831cb5da42b5b37185e0c8a526143b824"
      , _transactionRedeemerRedeemerDataHash = "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
      , _transactionRedeemerDatumHash = "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
      , _transactionRedeemerUnitMem = 1700
      , _transactionRedeemerUnitSteps = 476468
      , _transactionRedeemerFee = 172033
      }

-- | Information about (de-)registration of a stake address
-- within a transaction
data TransactionStake = TransactionStake
  { _transactionStakeCertIndex    :: Integer -- ^ Index of the certificate within the transaction
  , _transactionStakeAddress      :: Address -- ^ Delegation stake address
  , _transactionStakeRegistration :: Bool -- ^ Registration boolean, false if deregistration
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionStake", CamelToSnake]] TransactionStake

instance ToSample TransactionStake where
  toSamples = pure $ singleSample
    TransactionStake
      { _transactionStakeCertIndex = 0
      , _transactionStakeAddress = "stake1u9t3a0tcwune5xrnfjg4q7cpvjlgx9lcv0cuqf5mhfjwrvcwrulda"
      , _transactionStakeRegistration = True
      }

-- | Information about delegation certificates of a specific transaction
data TransactionDelegation = TransactionDelegation
  { _transactionDelegationCertIndex   :: Integer -- ^ Index of the certificate within the transaction
  , _transactionDelegationAddress     :: Address -- ^ Delegation stake address
  , _transactionDelegationPoolId      :: PoolId -- ^ Bech32 ID of delegated stake pool
  , _transactionDelegationActiveEpoch :: Epoch -- ^ Epoch in which the delegation becomes active
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionDelegation", CamelToSnake]] TransactionDelegation

instance ToSample TransactionDelegation where
  toSamples = pure $ singleSample
    TransactionDelegation
      { _transactionDelegationCertIndex = 0
      , _transactionDelegationAddress = "stake1u9t3a0tcwune5xrnfjg4q7cpvjlgx9lcv0cuqf5mhfjwrvcwrulda"
      , _transactionDelegationPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _transactionDelegationActiveEpoch = 210
      }

-- | Information about withdrawals of a specific transaction
data TransactionWithdrawal = TransactionWithdrawal
  { _transactionWithdrawalAddress :: Address -- ^ Bech32 withdrawal address
  , _transactionWithdrawalAmount  :: Lovelaces -- ^ Withdrawal amount in Lovelaces
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionWithdrawal", CamelToSnake]] TransactionWithdrawal

instance ToSample TransactionWithdrawal where
  toSamples = pure $ singleSample
    TransactionWithdrawal
      { _transactionWithdrawalAddress = "stake1u9r76ypf5fskppa0cmttas05cgcswrttn6jrq4yd7jpdnvc7gt0yc"
      , _transactionWithdrawalAmount = 431833601
      }

-- | Pot from which MIRs are transferred
data Pot = Reserve | Treasury
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ConstructorTagModifier '[ToLower]] Pot

instance ToSample Pot where
  toSamples = pure $ samples [ Reserve, Treasury ]

-- | Information about Move Instantaneous Rewards (MIRs) of a specific transaction
data TransactionMir = TransactionMir
  { _transactionMirPot       :: Pot -- ^ Source of MIR funds
  , _transactionMirCertIndex :: Integer -- ^ Index of the certificate within the transaction
  , _transactionMirAddress   :: Address -- ^ Bech32 stake address
  , _transactionMirAmount    :: Lovelaces -- ^ MIR amount in Lovelaces
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionMir", CamelToSnake]] TransactionMir

instance ToSample TransactionMir where
  toSamples = pure $ singleSample
    TransactionMir
      { _transactionMirPot = Reserve
      , _transactionMirCertIndex = 0
      , _transactionMirAddress = "stake1u9r76ypf5fskppa0cmttas05cgcswrttn6jrq4yd7jpdnvc7gt0yc"
      , _transactionMirAmount = 431833601
      }

-- | Information about stake pool registration and update certificates
-- of a specific transaction
data TransactionPoolUpdate = TransactionPoolUpdate
  { _transactionPoolUpdateCertIndex     :: Integer -- ^ Index of the certificate within the transaction
  , _transactionPoolUpdatePoolId        :: PoolId -- ^ Bech32 encoded pool ID
  , _transactionPoolUpdateVrfKey        :: Text -- ^ VRF key hash
  , _transactionPoolUpdatePledge        :: Lovelaces -- ^ Stake pool certificate pledge in Lovelaces
  , _transactionPoolUpdateMarginCost    :: Double -- ^  Margin tax cost of the stake pool
  , _transactionPoolUpdateFixedCost     :: Lovelaces -- ^ Fixed tax cost of the stake pool in Lovelaces
  , _transactionPoolUpdateRewardAccount :: Address -- ^ Bech32 reward account of the stake pool
  , _transactionPoolUpdateOwners        :: [Address]
  , _transactionPoolUpdateMetadata      ::  Maybe PoolUpdateMetadata
  , _transactionPoolUpdateRelays        :: [PoolRelay]
  , _transactionPoolUpdateActiveEpoch   :: Epoch -- ^ Epoch that the delegation becomes active
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionPoolUpdate", CamelToSnake]] TransactionPoolUpdate

instance ToSample TransactionPoolUpdate where
  toSamples = pure $ singleSample
    TransactionPoolUpdate
      { _transactionPoolUpdateCertIndex = 0
      , _transactionPoolUpdatePoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _transactionPoolUpdateVrfKey = "0b5245f9934ec2151116fb8ec00f35fd00e0aa3b075c4ed12cce440f999d8233"
      , _transactionPoolUpdatePledge = 5000000000
      , _transactionPoolUpdateMarginCost = 0.05
      , _transactionPoolUpdateFixedCost = 340000000
      , _transactionPoolUpdateRewardAccount = "stake1uxkptsa4lkr55jleztw43t37vgdn88l6ghclfwuxld2eykgpgvg3f"
      , _transactionPoolUpdateOwners = [ "stake1u98nnlkvkk23vtvf9273uq7cph5ww6u2yq2389psuqet90sv4xv9v" ]
      , _transactionPoolUpdateMetadata = Just samplePoolUpdateMetadata
      , _transactionPoolUpdateRelays = [ samplePoolRelay ]
      , _transactionPoolUpdateActiveEpoch = 210
      }

-- | Information about stake pool retirements
-- within a specific transaction
data TransactionPoolRetiring = TransactionPoolRetiring
  { _transactionPoolRetiringCertIndex     :: Integer -- ^ Index of the certificate within the transaction
  , _transactionPoolRetiringPoolId        :: PoolId -- ^ Bech32 stake pool ID
  , _transactionPoolRetiringRetiringEpoch :: Epoch -- ^ Retiring epoch
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionPoolRetiring", CamelToSnake]] TransactionPoolRetiring

instance ToSample TransactionPoolRetiring where
  toSamples = pure $ singleSample
    TransactionPoolRetiring
      { _transactionPoolRetiringCertIndex = 0
      , _transactionPoolRetiringPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _transactionPoolRetiringRetiringEpoch = 216
      }

-- | Transaction metadata in JSON
data TransactionMetaJSON = TransactionMetaJSON
  { _transactionMetaJSONLabel        :: Text -- ^ Metadata label
  , _transactionMetaJSONJSONMetadata :: Maybe Value -- ^ Content of the JSON metadata
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionMetaJSON", CamelToSnake]] TransactionMetaJSON

instance ToSample TransactionMetaJSON where
  toSamples =
    let oracleMeta val =
          object [
            "ADAUSD" .=
              [ object [ "value" .= (val :: Text)
                       , "source" .= ("ergoOracles" :: Text) ]
              ]
          ]
    in pure $ samples
    [ TransactionMetaJSON
        "1967"
        (Just $ object
           [ "metadata" .= ("https://nut.link/metadata.json" :: Text)
           , "hash" .= ("6bf124f217d0e5a0a8adb1dbd8540e1334280d49ab861127868339f43b3948af" :: Text)
           ])
    , TransactionMetaJSON
        "1968"
        (Just $ oracleMeta "0.15409850555139935")
    ]

-- | Transaction metadata in CBOR
data TransactionMetaCBOR = TransactionMetaCBOR
  { _transactionMetaCBORLabel        :: Text -- ^ Metadata label
  , _transactionMetaCBORMetadata     :: Maybe Text -- ^ Content of the CBOR metadata
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transactionMetaCBOR", CamelToSnake]] TransactionMetaCBOR

instance ToSample TransactionMetaCBOR where
  toSamples = pure $ singleSample $
    TransactionMetaCBOR
      "1968"
      (Just "a100a16b436f6d62696e6174696f6e8601010101010c")

-- | Update of a pool metadata
data PoolUpdateMetadata = PoolUpdateMetadata
  { _poolUpdateMetadataUrl         :: Maybe Text -- ^ URL to the stake pool metadata
  , _poolUpdateMetadataHash        :: Maybe Text -- ^ Hash of the metadata file
  , _poolUpdateMetadataTicker      :: Maybe Text -- ^ Ticker of the stake pool
  , _poolUpdateMetadataName        :: Maybe Text -- ^ Name of the stake pool
  , _poolUpdateMetadataDescription :: Maybe Text -- ^ Description of the stake pool
  , _poolUpdateMetadataHomepage    :: Maybe Text -- ^ Home page of the stake pool
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolUpdateMetadata", CamelToSnake]] PoolUpdateMetadata

-- Note: Similar to PoolMetadata but w/o PoolId and Hex fields

instance ToSample PoolUpdateMetadata where
  toSamples = pure $ singleSample samplePoolUpdateMetadata

samplePoolUpdateMetadata :: PoolUpdateMetadata
samplePoolUpdateMetadata =
  PoolUpdateMetadata
    { _poolUpdateMetadataUrl = Just "https://stakenuts.com/mainnet.json"
    , _poolUpdateMetadataHash = Just "47c0c68cb57f4a5b4a87bad896fc274678e7aea98e200fa14a1cb40c0cab1d8c"
    , _poolUpdateMetadataTicker = Just "NUTS"
    , _poolUpdateMetadataName = Just "Stake Nuts"
    , _poolUpdateMetadataDescription = Just "The best pool ever"
    , _poolUpdateMetadataHomepage = Just "https://stakentus.com/"
    }
