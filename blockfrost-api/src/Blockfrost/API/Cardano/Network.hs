-- | Cardano Network endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Network
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Network

data NetworkAPI route =
  NetworkAPI
    {
      _networkInfo
        :: route
        :- Summary "Network information"
        :> Description "Return detailed network information."
        :> Get '[JSON] Network
    , _networkEras
        :: route
        :- Summary "Query summary of blockchain eras"
        :> Description "Returns start and end of each era along with parameters that can vary between hard forks."
        :> "eras"
        :> Get '[JSON] [NetworkEraSummary]
    } deriving (Generic)
