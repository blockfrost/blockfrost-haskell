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


epochsClient :: MonadBlockfrost m => Project -> EpochsAPI (AsClientT m)
epochsClient = fromServant . _epochs . cardanoClient

getLatestEpoch_ :: MonadBlockfrost m => Project -> m EpochInfo
getLatestEpoch_ = _latestEpoch . epochsClient

-- | Get the information about the latest, therefore current, epoch.
getLatestEpoch :: MonadBlockfrost m => m EpochInfo
getLatestEpoch = go getLatestEpoch_

getLatestEpochProtocolParams_ :: MonadBlockfrost m => Project -> m ProtocolParams
getLatestEpochProtocolParams_ = _latestEpochProtocolParams . epochsClient

-- | Get the protocol parameters for the latest epoch.
getLatestEpochProtocolParams :: MonadBlockfrost m => m ProtocolParams
getLatestEpochProtocolParams = go getLatestEpochProtocolParams_

getEpoch_ :: MonadBlockfrost m => Project -> Epoch -> m EpochInfo
getEpoch_ = _getEpoch . epochsClient

-- | Get the information about specific epoch.
getEpoch :: MonadBlockfrost m => Epoch -> m EpochInfo
getEpoch e = go (`getEpoch_` e)

getNextEpochs_ :: MonadBlockfrost m => Project -> Epoch -> Paged -> m [EpochInfo]
getNextEpochs_ = _getNextEpochs . epochsClient

-- | Return the list of epochs following a specific epoch.
-- Allows custom paging using 'Paged'.
getNextEpochs' :: MonadBlockfrost m => Epoch -> Paged -> m [EpochInfo]
getNextEpochs' e pg = go (\p -> getNextEpochs_ p e pg)

-- | Return the list of epochs following a specific epoch.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getNextEpochs :: MonadBlockfrost m => Epoch -> m [EpochInfo]
getNextEpochs e = getNextEpochs' e def

getPreviousEpochs_ :: MonadBlockfrost m => Project -> Epoch -> Paged -> m [EpochInfo]
getPreviousEpochs_ = _getPreviousEpochs . epochsClient

-- | Return the list of epochs preceding a specific epoch.
-- Allows custom paging using 'Paged'.
getPreviousEpochs' :: MonadBlockfrost m => Epoch -> Paged -> m [EpochInfo]
getPreviousEpochs' e pg = go (\p -> getPreviousEpochs_ p e pg)

-- | Return the list of epochs preceding a specific epoch.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getPreviousEpochs :: MonadBlockfrost m => Epoch -> m [EpochInfo]
getPreviousEpochs e = getPreviousEpochs' e def

getEpochStake_ :: MonadBlockfrost m => Project -> Epoch -> Paged -> m [StakeDistribution]
getEpochStake_ = _getEpochStake . epochsClient

-- | Return the active stake distribution for the specified epoch.
-- Allows custom paging using 'Paged'.
getEpochStake' :: MonadBlockfrost m => Epoch -> Paged -> m [StakeDistribution]
getEpochStake' e pg = go (\p -> getEpochStake_ p e pg)

-- | Return the active stake distribution for the specified epoch.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getEpochStake :: MonadBlockfrost m => Epoch -> m [StakeDistribution]
getEpochStake e = getEpochStake' e def

getEpochStakeByPool_ :: MonadBlockfrost m => Project -> Epoch -> PoolId -> Paged -> m [PoolStakeDistribution]
getEpochStakeByPool_ = _getEpochStakeByPool . epochsClient

-- | Return the active stake distribution for the epoch specified by stake pool.
-- Allows custom paging using 'Paged'.
getEpochStakeByPool' :: MonadBlockfrost m => Epoch -> PoolId -> Paged -> m [PoolStakeDistribution]
getEpochStakeByPool' e i pg = go (\p -> getEpochStakeByPool_ p e i pg)

-- | Return the active stake distribution for the epoch specified by stake pool.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getEpochStakeByPool :: MonadBlockfrost m => Epoch -> PoolId -> m [PoolStakeDistribution]
getEpochStakeByPool e i = getEpochStakeByPool' e i def

getEpochBlocks_ :: MonadBlockfrost m => Project -> Epoch -> Paged -> SortOrder -> m [BlockHash]
getEpochBlocks_ = _getEpochBlocks . epochsClient

-- | Return the blocks minted for the specified epoch.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getEpochBlocks' :: MonadBlockfrost m => Epoch -> Paged -> SortOrder -> m [BlockHash]
getEpochBlocks' e pg s = go (\p -> getEpochBlocks_ p e pg s)

-- | Return the blocks minted for the specified epoch.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getEpochBlocks :: MonadBlockfrost m => Epoch -> m [BlockHash]
getEpochBlocks e = getEpochBlocks' e def def

getEpochBlocksByPool_ :: MonadBlockfrost m => Project -> Epoch -> PoolId -> Paged -> SortOrder -> m [BlockHash]
getEpochBlocksByPool_ = _getEpochBlocksByPool . epochsClient

-- | Return the block minted for the epoch specified by stake pool.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getEpochBlocksByPool' :: MonadBlockfrost m => Epoch -> PoolId -> Paged -> SortOrder -> m [BlockHash]
getEpochBlocksByPool' e i pg s = go (\p -> getEpochBlocksByPool_ p e i pg s)

-- | Return the block minted for the epoch specified by stake pool.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getEpochBlocksByPool :: MonadBlockfrost m => Epoch -> PoolId -> m [BlockHash]
getEpochBlocksByPool e i = getEpochBlocksByPool' e i def def

getEpochProtocolParams_ :: MonadBlockfrost m => Project -> Epoch -> m ProtocolParams
getEpochProtocolParams_ = _getEpochProtocolParams . epochsClient

-- | Return the protocol parameters for the specified epoch.
getEpochProtocolParams :: MonadBlockfrost m => Epoch -> m ProtocolParams
getEpochProtocolParams e = go (`getEpochProtocolParams_` e)
