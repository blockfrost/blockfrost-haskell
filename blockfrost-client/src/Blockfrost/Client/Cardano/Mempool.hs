-- | Mempool queries

module Blockfrost.Client.Cardano.Mempool
  ( getMempoolTransactions
  , getMempoolTransaction 
  , getMempoolTransactionsByAddress
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

mempoolClient :: MonadBlockfrost m => Project -> MempoolAPI (AsClientT m)
mempoolClient = fromServant . _mempool . cardanoClient

getMempoolTransactions :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [TxHashObject]
getMempoolTransactions = _mempoolTransactions . mempoolClient 

getMempoolTransaction :: MonadBlockfrost m => Project -> TxHash -> m MempoolTransaction
getMempoolTransaction = _specificTransaction . mempoolClient  

getMempoolTransactionsByAddress :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [TxHashObject]
getMempoolTransactionsByAddress = _specificAddress . mempoolClient 

