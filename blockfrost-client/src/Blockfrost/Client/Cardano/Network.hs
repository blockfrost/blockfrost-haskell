-- | Network queries

module Blockfrost.Client.Cardano.Network
  ( getNetworkInfo
  , getNetworkEras
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types


networkClient :: MonadBlockfrost m => Project -> NetworkAPI (AsClientT m)
networkClient = fromServant . _network . cardanoClient

getNetworkInfo_ :: MonadBlockfrost m => Project -> m Network
getNetworkInfo_ = _networkInfo . networkClient

-- | Get detailed network information.
getNetworkInfo :: MonadBlockfrost m => m Network
getNetworkInfo = go getNetworkInfo_

getNetworkEras_ :: MonadBlockfrost m => Project -> m [NetworkEraSummary]
getNetworkEras_ = _networkEras . networkClient

-- | Get summarized information on each era in the network, including start, end, and variable era parameters.
getNetworkEras :: MonadBlockfrost m => m [NetworkEraSummary]
getNetworkEras = go getNetworkEras_
