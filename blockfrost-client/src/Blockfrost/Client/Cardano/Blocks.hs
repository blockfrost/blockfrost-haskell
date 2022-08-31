-- | Block queries

module Blockfrost.Client.Cardano.Blocks
  ( getLatestBlock
  , getLatestBlockTxs
  , getLatestBlockTxs'
  , getBlock
  , getBlockSlot
  , getBlockEpochSlot
  , getNextBlocks
  , getNextBlocks'
  , getPreviousBlocks
  , getPreviousBlocks'
  , getBlockTxs
  , getBlockTxs'
  , getBlockAffectedAddresses'
  , getBlockAffectedAddresses
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

blocksClient :: MonadBlockfrost m => Project -> BlocksAPI (AsClientT m)
blocksClient = fromServant . _blocks . cardanoClient

getLatestBlock_ :: MonadBlockfrost m => Project -> m Block
getLatestBlock_ = _latest . blocksClient

-- | Return the latest block available to the backends, also known as the tip of the blockchain.
getLatestBlock :: MonadBlockfrost m => m Block
getLatestBlock = go getLatestBlock_

getLatestBlockTxs_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [TxHash]
getLatestBlockTxs_ = _latestTxs . blocksClient

-- | Return the transactions within the latest block.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getLatestBlockTxs' :: MonadBlockfrost m => Paged -> SortOrder -> m [TxHash]
getLatestBlockTxs' pg s = go (\p -> getLatestBlockTxs_ p pg s)

-- | Return the transactions within the latest block.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getLatestBlockTxs :: MonadBlockfrost m => m [TxHash]
getLatestBlockTxs = getLatestBlockTxs' def def

getBlock_ :: MonadBlockfrost m => Project -> Either Integer BlockHash -> m Block
getBlock_ = _block . blocksClient

-- | Return the content of a requested block.
getBlock :: MonadBlockfrost m => Either Integer BlockHash -> m Block
getBlock a = go (`getBlock_` a)

getBlockSlot_ :: MonadBlockfrost m => Project -> Slot -> m Block
getBlockSlot_ = __blockSlot . blocksClient

-- | Return the content of a requested block for a specific slot.
getBlockSlot :: MonadBlockfrost m => Slot -> m Block
getBlockSlot i = go (`getBlockSlot_` i)

getBlockEpochSlot_ :: MonadBlockfrost m => Project -> Epoch -> Slot -> m Block
getBlockEpochSlot_ = __blockEpochSlot . blocksClient

-- | Return the content of a requested block for a specific slot in an epoch.
getBlockEpochSlot :: MonadBlockfrost m => Epoch -> Slot -> m Block
getBlockEpochSlot ep sl = go (\p -> getBlockEpochSlot_ p ep sl)

getNextBlocks_ :: MonadBlockfrost m => Project -> Either Integer BlockHash -> Paged -> m [Block]
getNextBlocks_ = _blockNext . blocksClient

-- | Return the list of blocks following a specific block.
-- Allows custom paging using 'Paged'.
getNextBlocks' :: MonadBlockfrost m => Either Integer BlockHash -> Paged -> m [Block]
getNextBlocks' a pg = go (\p -> getNextBlocks_ p a pg)

-- | Return the list of blocks following a specific block.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getNextBlocks :: MonadBlockfrost m => Either Integer BlockHash -> m [Block]
getNextBlocks a = getNextBlocks' a def

getPreviousBlocks_ :: MonadBlockfrost m => Project -> Either Integer BlockHash -> Paged -> m [Block]
getPreviousBlocks_ = _blockPrevious . blocksClient

-- | Return the list of blocks preceding a specific block.
-- Allows custom paging using 'Paged'.
getPreviousBlocks' :: MonadBlockfrost m => Either Integer BlockHash -> Paged -> m [Block]
getPreviousBlocks' a pg = go (\p -> getPreviousBlocks_ p a pg)

-- | Return the list of blocks preceding a specific block.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getPreviousBlocks :: MonadBlockfrost m => Either Integer BlockHash -> m [Block]
getPreviousBlocks a = getPreviousBlocks' a def

getBlockTxs_ :: MonadBlockfrost m => Project -> Either Integer BlockHash -> Paged -> SortOrder -> m [TxHash]
getBlockTxs_ = _blockTxs . blocksClient

-- | Return the transactions within the block.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getBlockTxs' :: MonadBlockfrost m => Either Integer BlockHash -> Paged -> SortOrder -> m [TxHash]
getBlockTxs' a pg s = go (\p -> getBlockTxs_ p a pg s)

-- | Return the transactions within the block.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getBlockTxs :: MonadBlockfrost m => Either Integer BlockHash -> m [TxHash]
getBlockTxs a = getBlockTxs' a def def

getBlockAffectedAddresses_ :: MonadBlockfrost m => Project -> Either Integer BlockHash -> Paged -> m [(Address, [TxHash])]
getBlockAffectedAddresses_ = _blockAffectedAddresses . blocksClient

-- | Return list of addresses affected in the specified block with additional information, sorted by the bech32 address, ascending.
-- Allows custom paging using 'Paged'.
getBlockAffectedAddresses' :: MonadBlockfrost m => Either Integer BlockHash -> Paged -> m [(Address, [TxHash])]
getBlockAffectedAddresses' a pg = go (\p -> getBlockAffectedAddresses_ p a pg)

-- | Return list of addresses affected in the specified block with additional information, sorted by the bech32 address, ascending.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getBlockAffectedAddresses :: MonadBlockfrost m => Either Integer BlockHash -> m [(Address, [TxHash])]
getBlockAffectedAddresses a = getBlockAffectedAddresses' a def
