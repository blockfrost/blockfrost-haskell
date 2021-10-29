-- | Cardano Pools endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Transactions
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Transactions
import Blockfrost.Types.Shared

data TransactionsAPI route =
  TransactionsAPI
    {
      _tx
        :: route
        :- Summary "Specific transaction"
        :> Description "Return content of the requested transaction."
        :> Capture "hash" TxHash
        :> Get '[JSON] Transaction
    , _txUtxos
        :: route
        :- Summary "Transaction UTXOs"
        :> Description "Return the inputs and UTXOs of the specific transaction."
        :> Capture "hash" TxHash
        :> "utxos"
        :> Get '[JSON] TransactionUtxos
    , _txRedeemers
        :: route
        :- Summary "Transaction redeemers"
        :> Description "Obtain the transaction redeemers."
        :> Capture "hash" TxHash
        :> "redeemers"
        :> Get '[JSON] [TransactionRedeemer]
    , _txStakes
        :: route
        :- Summary "Transaction stake addresses certificates "
        :> Description "Obtain information about (de)registration of stake addresses within a transaction."
        :> Capture "hash" TxHash
        :> "stakes"
        :> Get '[JSON] [TransactionStake]
    , _txDelegations
        :: route
        :- Summary "Transaction delegation certificates"
        :> Description "Obtain information about delegation certificates of a specific transaction."
        :> Capture "hash" TxHash
        :> "delegations"
        :> Get '[JSON] [TransactionDelegation]
    , _txWithdrawals
        :: route
        :- Summary "Transaction withdrawal"
        :> Description "Obtain information about withdrawals of a specific transaction."
        :> Capture "hash" TxHash
        :> "withdrawals"
        :> Get '[JSON] [TransactionWithdrawal]
    , _txMirs
        :: route
        :- Summary "Transaction MIRs"
        :> Description "Obtain information about Move Instantaneous Rewards (MIRs) of a specific transaction."
        :> Capture "hash" TxHash
        :> "mirs"
        :> Get '[JSON] [TransactionMir]
    , _txPoolUpdates
        :: route
        :- Summary "Transaction stake pool registration and update certificates"
        :> Description "Obtain information about stake pool registration and update certificates of a specific transaction."
        :> Capture "hash" TxHash
        :> "pool_updates"
        :> Get '[JSON] [TransactionPoolUpdate]
    , _txPoolRetiring
        :: route
        :- Summary "Transaction stake pool retirement certificates"
        :> Description "Obtain information about stake pool retirements within a specific transaction."
        :> Capture "hash" TxHash
        :> "pool_retires"
        :> Get '[JSON] [TransactionPoolRetiring]
    , _txMetadataJSON
        :: route
        :- Summary "Transaction metadata"
        :> Description "Obtain the transaction metadata."
        :> Capture "hash" TxHash
        :> "metadata"
        :> Get '[JSON] [TransactionMetaJSON]
    , _txMetadataCBOR
        :: route
        :- Summary "Transaction metadata in CBOR"
        :> Description "Obtain the transaction metadata in CBOR."
        :> Capture "hash" TxHash
        :> "metadata"
        :> "cbor"
        :> Get '[JSON] [TransactionMetaCBOR]
    } deriving (Generic)
