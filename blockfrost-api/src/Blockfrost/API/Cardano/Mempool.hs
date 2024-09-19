-- | Cardano Mempool endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Mempool
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data MempoolAPI route =
  MempoolAPI
    {
      _mempoolTransactions
        :: route
        :- Summary "Transactions in Mempool."
        :> Description "Tx hash list of all transactions that are currently stored in the mempool."
        :> Pagination
        :> Sorting
        :> Get '[JSON] [TxHashObject]
    , _specificTransaction
        :: route
        :- Summary "Transaction in mempoool."
        :> Description "Content of a specific transaction in the mempool."
        :> Capture "hash" TxHash
        :> Get '[JSON] MempoolTransaction
    , _specificAddress
        :: route
        :- Summary "Transactions involving an address in mempool."
        :> Description "List of transactions in the mempool that involves a specific address."
        :> "addresses"
        :> Capture "address" Address
        :> Pagination
        :> Sorting
        :> Get '[JSON] [TxHashObject]
    } deriving (Generic)


