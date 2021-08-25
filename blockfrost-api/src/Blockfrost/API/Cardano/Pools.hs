-- | Cardano Pools endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Pools
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Pools
import Blockfrost.Types.Shared
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data PoolsAPI route =
  PoolsAPI
    {
      _listPools
        :: route
        :- Summary "List of stake pools"
        :> Description "List of registered stake pools."
        :> Pagination
        :> Sorting
        :> Get '[JSON] [PoolId]
    , _listRetiredPools
        :: route
        :- Summary "List of retired stake pools"
        :> Description "List of already retired stake pools."
        :> "retired"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [PoolEpoch]
    , _listRetiringPools
        :: route
        :- Summary "List of retiring stake pools"
        :> Description "List of stake pools retiring in the upcoming epochs"
        :> "retiring"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [PoolEpoch]
    , _getPool
        :: route
        :- Summary "Specific stake pool"
        :> Description "Pool information."
        :> Capture "pool_id" PoolId
        :> Get '[JSON] PoolInfo
   , _getPoolHistory
        :: route
        :- Summary "Stake pool history"
        :> Description "History of stake pool parameters over epochs."
        :> Capture "pool_id" PoolId
        :> "history"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [PoolHistory]
    , _getPoolMetadata
        :: route
        :- Summary "Stake pool metadata"
        :> Description "Stake pool registration metadata."
        :> Capture "pool_id" PoolId
        :> "metadata"
        :> Get '[JSON] (Maybe PoolMetadata)
    , _getPoolRelays
        :: route
        :- Summary "Stake pool relays"
        :> Description "Relays of a stake pool."
        :> Capture "pool_id" PoolId
        :> "relays"
        :> Get '[JSON] [PoolRelay]
    , _getPoolDelegators
        :: route
        :- Summary "Stake pool delegators"
        :> Description "List of current stake pools delegators."
        :> Capture "pool_id" PoolId
        :> "delegators"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [PoolDelegator]
    , _getPoolBlocks
        :: route
        :- Summary "Stake pool blocks"
        :> Description "List of stake pool blocks."
        :> Capture "pool_id" PoolId
        :> "blocks"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [BlockHash]
    , _getPoolUpdates
        :: route
        :- Summary "Stake pool updates"
        :> Description "List of certificate updates to the stake pool."
        :> Capture "pool_id" PoolId
        :> "updates"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [PoolUpdate]
    } deriving (Generic)
