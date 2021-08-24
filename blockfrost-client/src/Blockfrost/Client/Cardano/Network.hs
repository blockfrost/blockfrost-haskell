-- | Network queries

module Blockfrost.Client.Cardano.Network
  ( getNetworkInfo
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types


networkClient :: Project -> NetworkAPI (AsClientT BlockfrostClient)
networkClient = fromServant . _network . cardanoClient

getNetworkInfo_ :: Project -> BlockfrostClient Network
getNetworkInfo_ = _networkInfo . networkClient

-- | Get detailed network information.
getNetworkInfo:: BlockfrostClient Network
getNetworkInfo = go getNetworkInfo_
