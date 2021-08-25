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

poolsClient :: Project -> PoolsAPI (AsClientT BlockfrostClient)
poolsClient = fromServant . _pools . cardanoClient

listPools_ :: Project -> Paged -> SortOrder -> BlockfrostClient [PoolId]
listPools_ = _listPools . poolsClient

-- | List registered stake pools.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
listPools' :: Paged -> SortOrder -> BlockfrostClient [PoolId]
listPools' pg s = go (\p -> listPools_ p pg s)

-- | List registered stake pools.
listPools :: BlockfrostClient [PoolId]
listPools = listPools' def def

listRetiredPools_ :: Project -> Paged -> SortOrder -> BlockfrostClient [PoolEpoch]
listRetiredPools_ = _listRetiredPools . poolsClient

-- | List retired stake pools.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
listRetiredPools' :: Paged -> SortOrder -> BlockfrostClient [PoolEpoch]
listRetiredPools' pg s = go (\p -> listRetiredPools_ p pg s)

-- | List retired stake pools.
listRetiredPools :: BlockfrostClient [PoolEpoch]
listRetiredPools = listRetiredPools' def def

listRetiringPools_ :: Project -> Paged -> SortOrder -> BlockfrostClient [PoolEpoch]
listRetiringPools_ = _listRetiringPools . poolsClient

-- | List retiring stake pools.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
listRetiringPools' :: Paged -> SortOrder -> BlockfrostClient [PoolEpoch]
listRetiringPools' pg s = go (\p -> listRetiringPools_ p pg s)

-- | List retiring stake pools.
listRetiringPools :: BlockfrostClient [PoolEpoch]
listRetiringPools = listRetiringPools' def def

getPool_ :: Project -> PoolId -> BlockfrostClient PoolInfo
getPool_ = _getPool . poolsClient

-- | Get specific stake pool information
getPool:: PoolId -> BlockfrostClient PoolInfo
getPool p = go (`getPool_` p)

getPoolHistory_ :: Project -> PoolId -> Paged -> SortOrder -> BlockfrostClient [PoolHistory]
getPoolHistory_ = _getPoolHistory . poolsClient

-- | Get stake pool history
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getPoolHistory' :: PoolId -> Paged -> SortOrder -> BlockfrostClient [PoolHistory]
getPoolHistory' pid pg s = go (\p -> getPoolHistory_ p pid pg s)

-- | Get stake pool history
getPoolHistory :: PoolId -> BlockfrostClient [PoolHistory]
getPoolHistory p = getPoolHistory' p def def

getPoolMetadata_ :: Project -> PoolId -> BlockfrostClient (Maybe PoolMetadata)
getPoolMetadata_ = _getPoolMetadata . poolsClient

-- | Get stake pool metadata
getPoolMetadata :: PoolId -> BlockfrostClient (Maybe PoolMetadata)
getPoolMetadata p = go (`getPoolMetadata_` p)

getPoolRelays_ :: Project -> PoolId -> BlockfrostClient [PoolRelay]
getPoolRelays_ = _getPoolRelays . poolsClient

-- | Get stake pool relays
getPoolRelays :: PoolId -> BlockfrostClient [PoolRelay]
getPoolRelays p = go (`getPoolRelays_` p)

getPoolDelegators_ :: Project -> PoolId -> Paged -> SortOrder -> BlockfrostClient [PoolDelegator]
getPoolDelegators_ = _getPoolDelegators . poolsClient

-- | Get stake pool delegators
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getPoolDelegators' :: PoolId -> Paged -> SortOrder -> BlockfrostClient [PoolDelegator]
getPoolDelegators' pid pg s = go (\p -> getPoolDelegators_ p pid pg s)

-- | Get stake pool delegators
getPoolDelegators :: PoolId -> BlockfrostClient [PoolDelegator]
getPoolDelegators p = getPoolDelegators' p def def

getPoolBlocks_ :: Project -> PoolId -> Paged -> SortOrder -> BlockfrostClient [BlockHash]
getPoolBlocks_ = _getPoolBlocks . poolsClient

-- | Get stake pool blocks
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getPoolBlocks' :: PoolId -> Paged -> SortOrder -> BlockfrostClient [BlockHash]
getPoolBlocks' pid pg s = go (\p -> getPoolBlocks_ p pid pg s)

-- | Get stake pool blocks
getPoolBlocks :: PoolId -> BlockfrostClient [BlockHash]
getPoolBlocks p = getPoolBlocks' p def def

getPoolUpdates_ :: Project -> PoolId -> Paged -> SortOrder -> BlockfrostClient [PoolUpdate]
getPoolUpdates_ = _getPoolUpdates . poolsClient

-- | Get stake pool updates
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getPoolUpdates' :: PoolId -> Paged -> SortOrder -> BlockfrostClient [PoolUpdate]
getPoolUpdates' pid pg s = go (\p -> getPoolUpdates_ p pid pg s)

-- | Get stake pool updates
getPoolUpdates :: PoolId -> BlockfrostClient [PoolUpdate]
getPoolUpdates p = getPoolUpdates' p def def
