-- | Transaction queries

module Blockfrost.Client.Cardano.Transactions
  ( getTx
  , getTxUtxos
  , getTxStakes
  , getTxDelegations
  , getTxWithdrawals
  , getTxMirs
  , getTxPoolUpdates
  , getTxPoolRetiring
  , getTxMetadataJSON
  , getTxMetadataCBOR
  , submitTx
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

transactionsClient :: Project -> TransactionsAPI (AsClientT BlockfrostClient)
transactionsClient = fromServant . _transactions . cardanoClient

getTx_ :: Project -> TxHash -> BlockfrostClient Transaction
getTx_ = _tx . transactionsClient

-- | Get specific transaction
getTx :: TxHash -> BlockfrostClient Transaction
getTx t = go (`getTx_` t)

getTxUtxos_ :: Project -> TxHash -> BlockfrostClient TransactionUtxos
getTxUtxos_ = _txUtxos . transactionsClient

-- | Get transaction UTXOs
getTxUtxos :: TxHash -> BlockfrostClient TransactionUtxos
getTxUtxos t = go (`getTxUtxos_` t)

getTxStakes_ :: Project -> TxHash -> BlockfrostClient [TransactionStake]
getTxStakes_ = _txStakes . transactionsClient

-- | Get transaction UTXOs
getTxStakes :: TxHash -> BlockfrostClient [TransactionStake]
getTxStakes t = go (`getTxStakes_` t)

getTxDelegations_ :: Project -> TxHash -> BlockfrostClient [TransactionDelegation]
getTxDelegations_ = _txDelegations . transactionsClient

-- | Get transaction delegation certificates
getTxDelegations :: TxHash -> BlockfrostClient [TransactionDelegation]
getTxDelegations t = go (`getTxDelegations_` t)

getTxWithdrawals_ :: Project -> TxHash -> BlockfrostClient [TransactionWithdrawal]
getTxWithdrawals_ = _txWithdrawals . transactionsClient

-- | Get transaction withdrawals
getTxWithdrawals :: TxHash -> BlockfrostClient [TransactionWithdrawal]
getTxWithdrawals t = go (`getTxWithdrawals_` t)

getTxMirs_ :: Project -> TxHash -> BlockfrostClient [TransactionMir]
getTxMirs_ = _txMirs . transactionsClient

-- | Get transaction MIRs (Move Instantaneous Rewards)
getTxMirs :: TxHash -> BlockfrostClient [TransactionMir]
getTxMirs t = go (`getTxMirs_` t)

getTxPoolUpdates_ :: Project -> TxHash -> BlockfrostClient [TransactionPoolUpdate]
getTxPoolUpdates_ = _txPoolUpdates . transactionsClient

-- | Get transaction stake pool registration and update certificates
getTxPoolUpdates :: TxHash -> BlockfrostClient [TransactionPoolUpdate]
getTxPoolUpdates t = go (`getTxPoolUpdates_` t)

getTxPoolRetiring_ :: Project -> TxHash -> BlockfrostClient [TransactionPoolRetiring]
getTxPoolRetiring_ = _txPoolRetiring . transactionsClient

-- | Get transaction stake pool retirement certificates
getTxPoolRetiring :: TxHash -> BlockfrostClient [TransactionPoolRetiring]
getTxPoolRetiring t = go (`getTxPoolRetiring_` t)

getTxMetadataJSON_ :: Project -> TxHash -> BlockfrostClient [TransactionMetaJSON]
getTxMetadataJSON_ = _txMetadataJSON . transactionsClient

-- | Get transaction metadata in JSON
getTxMetadataJSON :: TxHash -> BlockfrostClient [TransactionMetaJSON]
getTxMetadataJSON t = go (`getTxMetadataJSON_` t)

getTxMetadataCBOR_ :: Project -> TxHash -> BlockfrostClient [TransactionMetaCBOR]
getTxMetadataCBOR_ = _txMetadataCBOR . transactionsClient

-- | Get transaction metadata in CBOR
getTxMetadataCBOR :: TxHash -> BlockfrostClient [TransactionMetaCBOR]
getTxMetadataCBOR t = go (`getTxMetadataCBOR_` t)

submitTx_ :: Project -> CBORString -> BlockfrostClient TxHash
submitTx_ = _txSubmit . cardanoClient

-- | Submit an already serialized transaction to the network.
submitTx :: CBORString -> BlockfrostClient TxHash
submitTx txCbor = go (`submitTx_` txCbor)
