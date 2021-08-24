-- | Epoch queries

module Blockfrost.Client.Cardano.Epochs
  ( getLatestEpoch
  , getLatestEpochProtocolParams
  , getEpoch
  , getNextEpochs
  , getNextEpochs'
  , getPreviousEpochs
  , getPreviousEpochs'
  , getEpochStake
  , getEpochStake'
  , getEpochStakeByPool
  , getEpochStakeByPool'
  , getEpochBlocks
  , getEpochBlocks'
  , getEpochBlocksByPool
  , getEpochBlocksByPool'
  , getEpochProtocolParams
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types


epochsClient :: Project -> EpochsAPI (AsClientT BlockfrostClient)
epochsClient = fromServant . _epochs . cardanoClient

getLatestEpoch_ :: Project -> BlockfrostClient EpochInfo
getLatestEpoch_ = _latestEpoch . epochsClient

-- | Get the information about the latest, therefore current, epoch.
getLatestEpoch :: BlockfrostClient EpochInfo
getLatestEpoch = go getLatestEpoch_

getLatestEpochProtocolParams_ :: Project -> BlockfrostClient ProtocolParams
getLatestEpochProtocolParams_ = _latestEpochProtocolParams . epochsClient

-- | Get the protocol parameters for the latest epoch.
getLatestEpochProtocolParams :: BlockfrostClient ProtocolParams
getLatestEpochProtocolParams = go getLatestEpochProtocolParams_

getEpoch_ :: Project -> Epoch -> BlockfrostClient EpochInfo
getEpoch_ = _getEpoch . epochsClient

-- | Get the information about specific epoch.
getEpoch :: Epoch -> BlockfrostClient EpochInfo
getEpoch e = go (`getEpoch_` e)

getNextEpochs_ :: Project -> Epoch -> Paged -> BlockfrostClient [EpochInfo]
getNextEpochs_ = _getNextEpochs . epochsClient

-- | Return the list of epochs following a specific epoch.
-- Allows custom paging using @Paged@.
getNextEpochs' :: Epoch -> Paged -> BlockfrostClient [EpochInfo]
getNextEpochs' e pg = go (\p -> getNextEpochs_ p e pg)

-- | Return the list of epochs following a specific epoch.
getNextEpochs :: Epoch -> BlockfrostClient [EpochInfo]
getNextEpochs e = getNextEpochs' e def

getPreviousEpochs_ :: Project -> Epoch -> Paged -> BlockfrostClient [EpochInfo]
getPreviousEpochs_ = _getPreviousEpochs . epochsClient

-- | Return the list of epochs preceding a specific epoch.
-- Allows custom paging using @Paged@.
getPreviousEpochs' :: Epoch -> Paged -> BlockfrostClient [EpochInfo]
getPreviousEpochs' e pg = go (\p -> getPreviousEpochs_ p e pg)

-- | Return the list of epochs preceding a specific epoch.
getPreviousEpochs :: Epoch -> BlockfrostClient [EpochInfo]
getPreviousEpochs e = getPreviousEpochs' e def

getEpochStake_ :: Project -> Epoch -> Paged -> BlockfrostClient [StakeDistribution]
getEpochStake_ = _getEpochStake . epochsClient

-- | Return the active stake distribution for the specified epoch.
-- Allows custom paging using @Paged@.
getEpochStake' :: Epoch -> Paged -> BlockfrostClient [StakeDistribution]
getEpochStake' e pg = go (\p -> getEpochStake_ p e pg)

-- | Return the active stake distribution for the specified epoch.
getEpochStake :: Epoch -> BlockfrostClient [StakeDistribution]
getEpochStake e = getEpochStake' e def

getEpochStakeByPool_ :: Project -> Epoch -> PoolId -> Paged -> BlockfrostClient [StakeDistribution]
getEpochStakeByPool_ = _getEpochStakeByPool . epochsClient

-- | Return the active stake distribution for the epoch specified by stake pool.
-- Allows custom paging using @Paged@.
getEpochStakeByPool' :: Epoch -> PoolId -> Paged -> BlockfrostClient [StakeDistribution]
getEpochStakeByPool' e i pg = go (\p -> getEpochStakeByPool_ p e i pg)

-- | Return the active stake distribution for the epoch specified by stake pool.
getEpochStakeByPool :: Epoch -> PoolId -> BlockfrostClient [StakeDistribution]
getEpochStakeByPool e i = getEpochStakeByPool' e i def

getEpochBlocks_ :: Project -> Epoch -> Paged -> SortOrder -> BlockfrostClient [BlockHash]
getEpochBlocks_ = _getEpochBlocks . epochsClient

-- | Return the blocks minted for the epoch specified.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getEpochBlocks' :: Epoch -> Paged -> SortOrder -> BlockfrostClient [BlockHash]
getEpochBlocks' e pg s = go (\p -> getEpochBlocks_ p e pg s)

-- | Return the blocks minted for the epoch specified.
getEpochBlocks :: Epoch -> BlockfrostClient [BlockHash]
getEpochBlocks e = getEpochBlocks' e def def

getEpochBlocksByPool_ :: Project -> Epoch -> PoolId -> Paged -> SortOrder -> BlockfrostClient [BlockHash]
getEpochBlocksByPool_ = _getEpochBlocksByPool . epochsClient

-- | Return the block minted for the epoch specified by stake pool.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getEpochBlocksByPool' :: Epoch -> PoolId -> Paged -> SortOrder -> BlockfrostClient [BlockHash]
getEpochBlocksByPool' e i pg s = go (\p -> getEpochBlocksByPool_ p e i pg s)

-- | Return the block minted for the epoch specified by stake pool.
getEpochBlocksByPool :: Epoch -> PoolId -> BlockfrostClient [BlockHash]
getEpochBlocksByPool e i = getEpochBlocksByPool' e i def def

getEpochProtocolParams_ :: Project -> Epoch -> BlockfrostClient ProtocolParams
getEpochProtocolParams_ = _getEpochProtocolParams . epochsClient

-- | Return the protocol parameters for the specified epoch.
getEpochProtocolParams :: Epoch -> BlockfrostClient ProtocolParams
getEpochProtocolParams e = go (`getEpochProtocolParams_` e)
