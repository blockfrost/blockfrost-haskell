-- | Network queries

module Blockfrost.Client.Cardano.Network
  ( getNetworkInfo
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
