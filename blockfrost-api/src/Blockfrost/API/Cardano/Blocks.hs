-- | Blocks API endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Blocks
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Blocks
import Blockfrost.Types.Shared
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data BlocksAPI route =
  BlocksAPI
    {
      _latest
        :: route
        :- Summary "Latest block"
        :> Description "Return the latest block available to the backends, \
                        \also known as the tip of the blockchain."
        :> "latest"
        :> Get '[JSON] Block
    , _latestTxs
        :: route
        :- Summary "Latest block transactions"
        :> Description "Return the transactions within the latest block."
        :> "latest"
        :> "txs"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [TxHash]
     , _block
        :: route
        :- Summary "Latest block transactions"
        :> Description "Return the transactions within the latest block."
        :> Capture "hash_or_number" (Either Integer BlockHash)
        :> Get '[JSON] Block
     , __blockSlot
        :: route
        :- Summary "Specific block in a slot"
        :> Description "Return the content of a requested block for a specific slot."
        :> "slot"
        :> Capture "slot_number" Slot
        :> Get '[JSON] Block
     , __blockEpochSlot
        :: route
        :- Summary "Specific block in a slot in an epoch"
        :> Description "Return the content of a requested block for a specific slot in an epoch."
        :> "epoch"
        :> Capture "epoch_number" Epoch
        :> "slot"
        :> Capture "slot_number" Slot
        :> Get '[JSON] Block
      , _blockNext
        :: route
        :- Summary "Listing of next blocks"
        :> Description "Return the list of blocks following a specific block."
        :> Capture "hash_or_number" (Either Integer BlockHash)
        :> "next"
        :> Pagination
        :> Get '[JSON] [Block]
      , _blockPrevious
        :: route
        :- Summary "Listing of preious blocks"
        :> Description "Return the list of blocks preceeding a specific block."
        :> Capture "hash_or_number" (Either Integer BlockHash)
        :> "previous"
        :> Pagination
        :> Get '[JSON] [Block]
      , _blockTxs
        :: route
        :- Summary "Block transactions"
        :> Description "Return the transactions within the block."
        :> Capture "hash_or_number" (Either Integer BlockHash)
        :> "txs"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [TxHash]
      , _blockAffectedAddresses
        :: route
        :- Summary "Addresses affected in a specific block"
        :> Description "Return list of addresses affected in the specified block with additional information, \
                       \sorted by the bech32 address, ascending."
        :> Capture "hash_or_number" (Either Integer BlockHash)
        :> "addresses"
        :> Pagination
        :> Get '[JSON] [(Address, [TxHash])]
    } deriving (Generic)
