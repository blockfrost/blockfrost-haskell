-- | Pool queries

module Blockfrost.Client.Cardano.Pools
  ( listPools
  , listPools'
  , listRetiredPools
  , listRetiredPools'
  , listRetiringPools
  , listRetiringPools'
  , getPool
  , getPoolHistory
  , getPoolHistory'
  , getPoolMetadata
  , getPoolRelays
  , getPoolDelegators
  , getPoolDelegators'
  , getPoolBlocks
  , getPoolBlocks'
  , getPoolUpdates
  , getPoolUpdates'
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

poolsClient :: MonadBlockfrost m => Project -> PoolsAPI (AsClientT m)
poolsClient = fromServant . _pools . cardanoClient

listPools_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [PoolId]
listPools_ = _listPools . poolsClient

-- | List registered stake pools.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
listPools' :: MonadBlockfrost m => Paged -> SortOrder -> m [PoolId]
listPools' pg s = go (\p -> listPools_ p pg s)

-- | List registered stake pools.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
listPools :: MonadBlockfrost m => m [PoolId]
listPools = listPools' def def

listRetiredPools_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [PoolEpoch]
listRetiredPools_ = _listRetiredPools . poolsClient

-- | List retired stake pools.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
listRetiredPools' :: MonadBlockfrost m => Paged -> SortOrder -> m [PoolEpoch]
listRetiredPools' pg s = go (\p -> listRetiredPools_ p pg s)

-- | List retired stake pools.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
listRetiredPools :: MonadBlockfrost m => m [PoolEpoch]
listRetiredPools = listRetiredPools' def def

listRetiringPools_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [PoolEpoch]
listRetiringPools_ = _listRetiringPools . poolsClient

-- | List retiring stake pools.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
listRetiringPools' :: MonadBlockfrost m => Paged -> SortOrder -> m [PoolEpoch]
listRetiringPools' pg s = go (\p -> listRetiringPools_ p pg s)

-- | List retiring stake pools.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
listRetiringPools :: MonadBlockfrost m => m [PoolEpoch]
listRetiringPools = listRetiringPools' def def

getPool_ :: MonadBlockfrost m => Project -> PoolId -> m PoolInfo
getPool_ = _getPool . poolsClient

-- | Get specific stake pool information
getPool :: MonadBlockfrost m => PoolId -> m PoolInfo
getPool p = go (`getPool_` p)

getPoolHistory_ :: MonadBlockfrost m => Project -> PoolId -> Paged -> SortOrder -> m [PoolHistory]
getPoolHistory_ = _getPoolHistory . poolsClient

-- | Get stake pool history
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getPoolHistory' :: MonadBlockfrost m => PoolId -> Paged -> SortOrder -> m [PoolHistory]
getPoolHistory' pid pg s = go (\p -> getPoolHistory_ p pid pg s)

-- | Get stake pool history
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getPoolHistory :: MonadBlockfrost m => PoolId -> m [PoolHistory]
getPoolHistory p = getPoolHistory' p def def

getPoolMetadata_ :: MonadBlockfrost m => Project -> PoolId -> m (Maybe PoolMetadata)
getPoolMetadata_ = _getPoolMetadata . poolsClient

-- | Get stake pool metadata
getPoolMetadata :: MonadBlockfrost m => PoolId -> m (Maybe PoolMetadata)
getPoolMetadata p = go (`getPoolMetadata_` p)

getPoolRelays_ :: MonadBlockfrost m => Project -> PoolId -> m [PoolRelay]
getPoolRelays_ = _getPoolRelays . poolsClient

-- | Get stake pool relays
getPoolRelays :: MonadBlockfrost m => PoolId -> m [PoolRelay]
getPoolRelays p = go (`getPoolRelays_` p)

getPoolDelegators_ :: MonadBlockfrost m => Project -> PoolId -> Paged -> SortOrder -> m [PoolDelegator]
getPoolDelegators_ = _getPoolDelegators . poolsClient

-- | Get stake pool delegators
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getPoolDelegators' :: MonadBlockfrost m => PoolId -> Paged -> SortOrder -> m [PoolDelegator]
getPoolDelegators' pid pg s = go (\p -> getPoolDelegators_ p pid pg s)

-- | Get stake pool delegators
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getPoolDelegators :: MonadBlockfrost m => PoolId -> m [PoolDelegator]
getPoolDelegators p = getPoolDelegators' p def def

getPoolBlocks_ :: MonadBlockfrost m => Project -> PoolId -> Paged -> SortOrder -> m [BlockHash]
getPoolBlocks_ = _getPoolBlocks . poolsClient

-- | Get stake pool blocks
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getPoolBlocks' :: MonadBlockfrost m => PoolId -> Paged -> SortOrder -> m [BlockHash]
getPoolBlocks' pid pg s = go (\p -> getPoolBlocks_ p pid pg s)

-- | Get stake pool blocks
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getPoolBlocks :: MonadBlockfrost m => PoolId -> m [BlockHash]
getPoolBlocks p = getPoolBlocks' p def def

getPoolUpdates_ :: MonadBlockfrost m => Project -> PoolId -> Paged -> SortOrder -> m [PoolUpdate]
getPoolUpdates_ = _getPoolUpdates . poolsClient

-- | Get stake pool updates
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getPoolUpdates' :: MonadBlockfrost m => PoolId -> Paged -> SortOrder -> m [PoolUpdate]
getPoolUpdates' pid pg s = go (\p -> getPoolUpdates_ p pid pg s)

-- | Get stake pool updates
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getPoolUpdates :: MonadBlockfrost m => PoolId -> m [PoolUpdate]
getPoolUpdates p = getPoolUpdates' p def def
