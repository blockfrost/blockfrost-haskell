-- | Cardano Scripts endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Scripts
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Scripts
import Blockfrost.Types.Shared
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data ScriptsAPI route =
  ScriptsAPI
    {
      _listScripts
        :: route
        :- Summary "Scripts"
        :> Description "List of scripts."
        :> Pagination
        :> Sorting
        :> Get '[JSON] [ScriptHash]
    , _getScript
        :: route
        :- Summary "Specific scripts"
        :> Description "Information about a specific script."
        :> Capture "script_hash" ScriptHash
        :> Get '[JSON] Script
    , _getScriptRedeemers
        :: route
        :- Summary "Redeemers of a specific script"
        :> Description "List of redeemers of a specific script."
        :> Capture "script_hash" ScriptHash
        :> Pagination
        :> Sorting
        :> "redeemers"
        :> Get '[JSON] [ScriptRedeemer]
    } deriving (Generic)
