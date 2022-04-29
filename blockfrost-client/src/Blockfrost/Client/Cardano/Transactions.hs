-- | Transaction queries

module Blockfrost.Client.Cardano.Transactions
  ( getTx
  , getTxUtxos
  , getTxRedeemers
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

transactionsClient :: MonadBlockfrost m => Project -> TransactionsAPI (AsClientT m)
transactionsClient = fromServant . _transactions . cardanoClient

getTx_ :: MonadBlockfrost m => Project -> TxHash -> m Transaction
getTx_ = _tx . transactionsClient

-- | Get specific transaction
getTx :: MonadBlockfrost m => TxHash -> m Transaction
getTx t = go (`getTx_` t)

getTxUtxos_ :: MonadBlockfrost m => Project -> TxHash -> m TransactionUtxos
getTxUtxos_ = _txUtxos . transactionsClient

-- | Get transaction UTXOs
getTxUtxos :: MonadBlockfrost m => TxHash -> m TransactionUtxos
getTxUtxos t = go (`getTxUtxos_` t)

getTxRedeemers_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionRedeemer]
getTxRedeemers_ = _txRedeemers . transactionsClient

-- | Get transaction redeemers
getTxRedeemers :: MonadBlockfrost m => TxHash -> m [TransactionRedeemer]
getTxRedeemers t = go (`getTxRedeemers_` t)

getTxStakes_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionStake]
getTxStakes_ = _txStakes . transactionsClient

-- | Get (de-)registrations of a stake address within a transaction
getTxStakes :: MonadBlockfrost m => TxHash -> m [TransactionStake]
getTxStakes t = go (`getTxStakes_` t)

getTxDelegations_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionDelegation]
getTxDelegations_ = _txDelegations . transactionsClient

-- | Get transaction delegation certificates
getTxDelegations :: MonadBlockfrost m => TxHash -> m [TransactionDelegation]
getTxDelegations t = go (`getTxDelegations_` t)

getTxWithdrawals_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionWithdrawal]
getTxWithdrawals_ = _txWithdrawals . transactionsClient

-- | Get transaction withdrawals
getTxWithdrawals :: MonadBlockfrost m => TxHash -> m [TransactionWithdrawal]
getTxWithdrawals t = go (`getTxWithdrawals_` t)

getTxMirs_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionMir]
getTxMirs_ = _txMirs . transactionsClient

-- | Get transaction MIRs (Move Instantaneous Rewards)
getTxMirs :: MonadBlockfrost m => TxHash -> m [TransactionMir]
getTxMirs t = go (`getTxMirs_` t)

getTxPoolUpdates_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionPoolUpdate]
getTxPoolUpdates_ = _txPoolUpdates . transactionsClient

-- | Get transaction stake pool registration and update certificates
getTxPoolUpdates :: MonadBlockfrost m => TxHash -> m [TransactionPoolUpdate]
getTxPoolUpdates t = go (`getTxPoolUpdates_` t)

getTxPoolRetiring_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionPoolRetiring]
getTxPoolRetiring_ = _txPoolRetiring . transactionsClient

-- | Get transaction stake pool retirement certificates
getTxPoolRetiring :: MonadBlockfrost m => TxHash -> m [TransactionPoolRetiring]
getTxPoolRetiring t = go (`getTxPoolRetiring_` t)

getTxMetadataJSON_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionMetaJSON]
getTxMetadataJSON_ = _txMetadataJSON . transactionsClient

-- | Get transaction metadata in JSON
getTxMetadataJSON :: MonadBlockfrost m => TxHash -> m [TransactionMetaJSON]
getTxMetadataJSON t = go (`getTxMetadataJSON_` t)

getTxMetadataCBOR_ :: MonadBlockfrost m => Project -> TxHash -> m [TransactionMetaCBOR]
getTxMetadataCBOR_ = _txMetadataCBOR . transactionsClient

-- | Get transaction metadata in CBOR
getTxMetadataCBOR :: MonadBlockfrost m => TxHash -> m [TransactionMetaCBOR]
getTxMetadataCBOR t = go (`getTxMetadataCBOR_` t)

submitTx_ :: MonadBlockfrost m => Project -> CBORString -> m TxHash
submitTx_ = _txSubmit . cardanoClient

-- | Submit an already serialized transaction to the network.
submitTx :: MonadBlockfrost m => CBORString -> m TxHash
submitTx txCbor = go (`submitTx_` txCbor)
