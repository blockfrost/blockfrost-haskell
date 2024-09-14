-- | Transaction metadata

module Blockfrost.Types.Cardano.Mempool
  ( MempoolTransaction(..),
    TransactionInMempool (..), 
    MempoolTxAmount(..),
    MempoolUTxOInput(..),
    MempoolRedeemer(..),
  ) where

import Data.Text 
import Deriving.Aeson
import Blockfrost.Types.Cardano.Transactions
import Blockfrost.Types.Shared.Ada

data MempoolTransaction = MempoolTransaction 
  { _tx :: TransactionInMempool
  , _inputs :: [MempoolUTxOInput]
  , _outputs :: [UtxoOutput]
  , _redeemers :: Maybe [MempoolRedeemer]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_", CamelToSnake]] MempoolTransaction

data TransactionInMempool = TransactionInMempool
  { _transactionHash                 :: Text -- ^ Transaction hash
  , _transactionOutputAmount         :: [MempoolTxAmount] -- ^ Transaction outputs
  , _transactionFees                 :: Lovelaces -- ^ Transacction fee 
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

data MempoolTxAmount 
  = MempoolTxAmount 
  { _amountUnit :: Text -- ^ Quantity in Lovelaces
  , _amountQuantity :: Text -- ^ Quantity in the unit
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_amount", CamelToSnake]] MempoolTxAmount

data MempoolUTxOInput
  = MempoolUTxOInput
   { _address :: Text -- ^ Address
   , _txHash :: Text -- ^ Transaction hash
   , _outputIndex :: Integer -- ^ Output index
   , _collateral :: Bool -- ^ True if the input is a collateral input
   , _reference :: Bool -- ^ Is the input a reference input
   } 
    deriving stock (Show, Eq, Generic)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_", CamelToSnake]] MempoolUTxOInput

data MempoolRedeemer
  = MempoolRedeemer
  { _tx_index :: Integer -- ^ Transaction index
  , _purpose :: Text -- ^ Purpose of the redeemer
  , _unit_mem :: Text -- ^ Memory unit
  , _unit_steps :: Text -- ^ Steps unit
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_", CamelToSnake]] MempoolRedeemer