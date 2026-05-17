-- | Transaction metadata

module Blockfrost.Types.Cardano.Mempool
  ( MempoolTransaction(..)
  , mempoolTransactionSample
  , TransactionInMempool (..)
  , MempoolUTxOInput(..)
  , MempoolRedeemer(..)
  ) where

import Data.Text
import Deriving.Aeson
import Blockfrost.Types.Cardano.Transactions
import Blockfrost.Types.Shared.Ada
import Blockfrost.Types.Shared.Address
import Blockfrost.Types.Shared.Amount
import Blockfrost.Types.Shared.Quantity
import Blockfrost.Types.Shared.ValidationPurpose

import Servant.Docs (ToSample (..), singleSample)

data MempoolTransaction = MempoolTransaction
  { _mempoolTransactionTx :: TransactionInMempool
  , _mempoolTransactionInputs :: [MempoolUTxOInput]
  , _mempoolTransactionOutputs :: [UtxoOutput]
  , _mempoolTransactionRedeemers :: Maybe [MempoolRedeemer]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_mempoolTransaction", CamelToSnake]] MempoolTransaction

instance ToSample MempoolTransaction where
  toSamples = pure $ singleSample mempoolTransactionSample

mempoolTransactionSample :: MempoolTransaction
mempoolTransactionSample =
  MempoolTransaction
    { _mempoolTransactionTx = transactionInMempoolSample
    , _mempoolTransactionInputs = pure mempoolUTxOInputSample
    , _mempoolTransactionOutputs = pure utxoOutSample
    , _mempoolTransactionRedeemers = Just $ pure mempoolRedeemerSample
    }

data TransactionInMempool = TransactionInMempool
  { _transactionHash                 :: Text -- ^ Transaction hash
  , _transactionOutputAmount         :: [Amount] -- ^ Transaction outputs
  , _transactionFees                 :: Lovelaces -- ^ Transaction fee
  , _transactionDeposit              :: Lovelaces -- ^ Deposit within the transaction in Lovelaces
  , _transactionSize                 :: Integer -- ^ Size of the transaction in Bytes
  , _transactionInvalidBefore        :: Maybe Text -- ^ Left (included) endpoint of the timelock validity intervals
  , _transactionInvalidHereafter     :: Maybe Text -- ^ Right (excluded) endpoint of the timelock validity intervals
  , _transactionUtxoCount            :: Integer -- ^ Count of UTXOs within the transaction
  , _transactionWithdrawalCount      :: Integer -- ^ Count of the withdrawals within the transaction
  , _transactionMirCertCount         :: Integer -- ^ Count of the MIR certificates within the transaction
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
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_transaction", CamelToSnake]] TransactionInMempool

instance ToSample TransactionInMempool where
  toSamples = pure $ singleSample transactionInMempoolSample

transactionInMempoolSample :: TransactionInMempool
transactionInMempoolSample =
  TransactionInMempool
    { _transactionHash = "1e043f100dce12d107f679685acd2fc0610e10f72a92d412794c9773d11d8477"
    , _transactionOutputAmount = [ AdaAmount 42000000 ]
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

data MempoolUTxOInput = MempoolUTxOInput
   { _mempoolUTxOInputAddress     :: Maybe Address -- ^ Address
   , _mempoolUTxOInputTxHash      :: Text -- ^ Transaction hash
   , _mempoolUTxOInputOutputIndex :: Integer -- ^ Output index
   , _mempoolUTxOInputCollateral  :: Bool -- ^ True if the input is a collateral input
   , _mempoolUTxOInputReference   :: Bool -- ^ Is the input a reference input
   }
   deriving stock (Show, Eq, Generic)
   deriving (FromJSON, ToJSON)
   via CustomJSON '[FieldLabelModifier '[StripPrefix "_mempoolUTxOInput", CamelToSnake]] MempoolUTxOInput

instance ToSample MempoolUTxOInput where
  toSamples = pure $ singleSample mempoolUTxOInputSample

mempoolUTxOInputSample :: MempoolUTxOInput
mempoolUTxOInputSample =
  MempoolUTxOInput
    { _mempoolUTxOInputAddress = Just $ Address "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv"
    , _mempoolUTxOInputTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0"
    , _mempoolUTxOInputOutputIndex = 0
    , _mempoolUTxOInputCollateral = False
    , _mempoolUTxOInputReference = False
    }

data MempoolRedeemer = MempoolRedeemer
  { _mempoolRedeemerTxIndex   :: Integer -- ^ Transaction index
  , _mempoolRedeemerPurpose   :: ValidationPurpose -- ^ Purpose of the redeemer
  , _mempoolRedeemerUnitMem   :: Quantity -- ^ Memory unit
  , _mempoolRedeemerUnitSteps :: Quantity -- ^ Steps unit
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_mempoolRedeemer", CamelToSnake]] MempoolRedeemer

instance ToSample MempoolRedeemer where
  toSamples = pure $ singleSample mempoolRedeemerSample

mempoolRedeemerSample :: MempoolRedeemer
mempoolRedeemerSample =
  MempoolRedeemer
    { _mempoolRedeemerTxIndex = 0
    , _mempoolRedeemerPurpose = Spend
    , _mempoolRedeemerUnitMem = 1700
    , _mempoolRedeemerUnitSteps = 476468
    }
