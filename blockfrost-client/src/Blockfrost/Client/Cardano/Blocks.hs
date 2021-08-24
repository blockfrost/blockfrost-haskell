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
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

blocksClient :: Project -> BlocksAPI (AsClientT BlockfrostClient)
blocksClient = fromServant . _blocks . cardanoClient

getLatestBlock_ :: Project -> BlockfrostClient Block
getLatestBlock_ = _latest . blocksClient

-- | Return the latest block available to the backends, also known as the tip of the blockchain.
getLatestBlock :: BlockfrostClient Block
getLatestBlock = go getLatestBlock_

getLatestBlockTxs_ :: Project -> Paged -> SortOrder -> BlockfrostClient [TxHash]
getLatestBlockTxs_ = _latestTxs . blocksClient

-- | Return the transactions within the latest block.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getLatestBlockTxs' :: Paged -> SortOrder -> BlockfrostClient [TxHash]
getLatestBlockTxs' pg s = go (\p -> getLatestBlockTxs_ p pg s)

getLatestBlockTxs :: BlockfrostClient [TxHash]
getLatestBlockTxs = getLatestBlockTxs' def def

getBlock_ :: Project -> Either Integer BlockHash -> BlockfrostClient Block
getBlock_ = _block . blocksClient

-- | Return the content of a requested block.
getBlock :: Either Integer BlockHash -> BlockfrostClient Block
getBlock a = go (`getBlock_` a)

getBlockSlot_ :: Project -> Slot -> BlockfrostClient Block
getBlockSlot_ = __blockSlot . blocksClient

-- | Return the content of a requested block for a specific slot.
getBlockSlot :: Slot -> BlockfrostClient Block
getBlockSlot i = go (`getBlockSlot_` i)

getBlockEpochSlot_ :: Project -> Epoch -> Slot -> BlockfrostClient Block
getBlockEpochSlot_ = __blockEpochSlot . blocksClient

-- | Return the content of a requested block for a specific slot in an epoch.
getBlockEpochSlot :: Epoch -> Slot -> BlockfrostClient Block
getBlockEpochSlot ep sl = go (\p -> getBlockEpochSlot_ p ep sl)

getNextBlocks_ :: Project -> Either Integer BlockHash -> Paged -> BlockfrostClient [Block]
getNextBlocks_ = _blockNext . blocksClient

-- | Return the list of blocks following a specific block.
-- Allows custom paging using @Paged@.
getNextBlocks' :: Either Integer BlockHash -> Paged -> BlockfrostClient [Block]
getNextBlocks' a pg = go (\p -> getNextBlocks_ p a pg)

-- | Return the list of blocks following a specific block.
getNextBlocks :: Either Integer BlockHash -> BlockfrostClient [Block]
getNextBlocks a = getNextBlocks' a def

getPreviousBlocks_ :: Project -> Either Integer BlockHash -> Paged -> BlockfrostClient [Block]
getPreviousBlocks_ = _blockPrevious . blocksClient

-- | Return the list of blocks preceding a specific block.
-- Allows custom paging using @Paged@.
getPreviousBlocks' :: Either Integer BlockHash -> Paged -> BlockfrostClient [Block]
getPreviousBlocks' a pg = go (\p -> getPreviousBlocks_ p a pg)

-- | Return the list of blocks preceding a specific block.
getPreviousBlocks :: Either Integer BlockHash -> BlockfrostClient [Block]
getPreviousBlocks a = getPreviousBlocks' a def

getBlockTxs_ :: Project -> Either Integer BlockHash -> Paged -> SortOrder -> BlockfrostClient [TxHash]
getBlockTxs_ = _blockTxs . blocksClient

-- | Return the transactions within the block.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getBlockTxs' :: Either Integer BlockHash -> Paged -> SortOrder -> BlockfrostClient [TxHash]
getBlockTxs' a pg s = go (\p -> getBlockTxs_ p a pg s)

-- | Return the transactions within the block.
getBlockTxs :: Either Integer BlockHash -> BlockfrostClient [TxHash]
getBlockTxs a = getBlockTxs' a def def
