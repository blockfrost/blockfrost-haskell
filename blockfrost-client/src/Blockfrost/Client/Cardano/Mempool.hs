-- | Mempool queries

module Blockfrost.Client.Cardano.Mempool
  ( getMempoolTransactions
  , getMempoolTransactions'
  , getMempoolTransaction
  , getMempoolTransactionsByAddress
  , getMempoolTransactionsByAddress'
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

mempoolClient :: MonadBlockfrost m => Project -> MempoolAPI (AsClientT m)
mempoolClient = fromServant . _mempool . cardanoClient

getMempoolTransactions_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [TxHashObject]
getMempoolTransactions_ = _mempoolTransactions . mempoolClient

-- | Get a list of transactions currently in Blockfrost.io mempool
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getMempoolTransactions' :: MonadBlockfrost m => Paged -> SortOrder -> m [TxHashObject]
getMempoolTransactions' pg s = go (\p -> getMempoolTransactions_ p pg s)

-- | Get a list of transactions currently in Blockfrost.io mempool
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getMempoolTransactions :: MonadBlockfrost m => m [TxHashObject]
getMempoolTransactions = getMempoolTransactions' def def

getMempoolTransaction_ :: MonadBlockfrost m => Project -> TxHash -> m MempoolTransaction
getMempoolTransaction_ = _specificTransaction . mempoolClient

-- | Get details about a specific @MempoolTransaction@
getMempoolTransaction :: MonadBlockfrost m => TxHash -> m MempoolTransaction
getMempoolTransaction t = go (`getMempoolTransaction_` t)

getMempoolTransactionsByAddress_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [TxHashObject]
getMempoolTransactionsByAddress_ = _specificAddress . mempoolClient

-- | Get a list of transactions currently in Blockfrost.io mempool
-- where at least one of the transaction inputs or outputs belongs to the address.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getMempoolTransactionsByAddress' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [TxHashObject]
getMempoolTransactionsByAddress' addr pg s = go (\p -> getMempoolTransactionsByAddress_ p addr pg s)

-- | Get a list of transactions currently in Blockfrost.io mempool
-- where at least one of the transaction inputs or outputs belongs to the address.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getMempoolTransactionsByAddress :: MonadBlockfrost m => Address -> m [TxHashObject]
getMempoolTransactionsByAddress addr = go (\p -> getMempoolTransactionsByAddress_ p addr def def)
