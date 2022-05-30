-- | Epochs API endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Epochs
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Epochs
import Blockfrost.Types.Shared
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data EpochsAPI route =
  EpochsAPI
    {
      _latestEpoch
        :: route
        :- Summary "Latest epoch"
        :> Description "Return the information about the latest, therefore current, epoch."
        :> "latest"
        :> Get '[JSON] EpochInfo
    , _latestEpochProtocolParams
        :: route
        :- Summary "Latest epoch protocol parameters"
        :> Description "Return the protocol parameters for the latest epoch."
        :> "latest"
        :> "parameters"
        :> Get '[JSON] ProtocolParams
    , _getEpoch
        :: route
        :- Summary "Specific epoch"
        :> Description "Return the content of the requested epoch."
        :> Capture "epoch_number" Epoch
        :> Get '[JSON] EpochInfo
    , _getNextEpochs
        :: route
        :- Summary "List of next epochs"
        :> Description "Return the list of epochs following a specific epoch."
        :> Capture "epoch_number" Epoch
        :> "next"
        :> Pagination
        :> Get '[JSON] [EpochInfo]
     , _getPreviousEpochs
        :: route
        :- Summary "List of previous epochs"
        :> Description "Return the list of epochs preceding a specific epoch."
        :> Capture "epoch_number" Epoch
        :> "previous"
        :> Pagination
        :> Get '[JSON] [EpochInfo]
     , _getEpochStake
        :: route
        :- Summary "Stake distribution"
        :> Description "Return the active stake distribution for the specified epoch."
        :> Capture "epoch_number" Epoch
        :> "stakes"
        :> Pagination
        :> Get '[JSON] [StakeDistribution]
     , _getEpochStakeByPool
        :: route
        :- Summary "Stake distribution by pool"
        :> Description "Return the active stake distribution for the epoch specified by stake pool."
        :> Capture "epoch_number" Epoch
        :> "stakes"
        :> Capture "pool_id" PoolId
        :> Pagination
        :> Get '[JSON] [PoolStakeDistribution]
     , _getEpochBlocks
        :: route
        :- Summary "Block distribution"
        :> Description "Return the blocks minted for the epoch specified."
        :> Capture "epoch_number" Epoch
        :> "blocks"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [BlockHash]
     , _getEpochBlocksByPool
        :: route
        :- Summary "Block distribution by pool"
        :> Description "Return the block minted for the epoch specified by stake pool."
        :> Capture "epoch_number" Epoch
        :> "blocks"
        :> Capture "pool_id" PoolId
        :> Pagination
        :> Sorting
        :> Get '[JSON] [BlockHash]
    , _getEpochProtocolParams
        :: route
        :- Summary "Protocol parameters"
        :> Description "Return the protocol parameters for the specified epoch."
        :> Capture "epoch_number" Epoch
        :> "parameters"
        :> Get '[JSON] ProtocolParams
    } deriving (Generic)
