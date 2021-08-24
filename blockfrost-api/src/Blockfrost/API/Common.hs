-- | Common services

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Common
  where

import Blockfrost.Types
import Blockfrost.Util.Tag (Tag)
import Data.Text (Text)
import Servant.API
import Servant.API.Generic

data CommonAPI route =
  CommonAPI
    {
      _getRoot
        :: route
        :- Summary "Root endpoint"
        :> Description "Root endpoint has no other function than to point end users to documentation."
        :> Tag "Health"
        :> Get '[JSON] URLVersion
    , _getHealth
        :: route
        :- Summary "Backend health status"
        :> Description "Return backend status as a boolean. \
                        \Your application should handle situations when backend for the given chain is unavailable."
        :> Tag "Health"
        :> "health"
        :> Get '[JSON] Healthy
    , _getClock
        :: route
        :- Summary "Current backend time"
        :> Description "This endpoint provides the current UNIX time. \
                         \Your application might use this to verify if \
                         \the client clock is not out of sync."
        :> Tag "Health"
        :> "health"
        :> "clock"
        :> Get '[JSON] ServerTime
    , _metrics
        :: route
        :- Summary "Blockfrost usage metrics"
        :> Description "History of your Blockfrost usage metrics in the past 30 days."
        :> Tag "Metrics"
        :> "metrics"
        :> Get '[JSON] [Metric]
    , _metricsEndpoints
        :: route
        :- Summary "Blockfrost endpoint usage metrics"
        :> Description "History of your Blockfrost usage metrics per endpoint in the past 30 days."
        :> Tag "Metrics"
        :> "metrics"
        :> "endpoints"
        :> Get '[JSON] [(Text, Metric)]
    } deriving (Generic)
