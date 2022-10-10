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
        :> Get '[JSON] ScriptHashList
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
     , _getScriptDatum
        :: route
        :- Summary "Datum value"
        :> Description "Query JSON value of a datum by its hash"
        :> "datum"
        :> Capture "datum_hash" DatumHash
        :> Get '[JSON] ScriptDatum
     , _getScriptDatumCBOR
        :: route
        :- Summary "Datum CBOR value"
        :> Description "Query CBOR serialised datum by its hash"
        :> "datum"
        :> Capture "datum_hash" DatumHash
        :> "cbor"
        :> Get '[JSON] ScriptDatumCBOR
      , _getScriptJSON
        :: route
        :- Summary "Script JSON"
        :> Description "JSON representation of a `timelock` script"
        :> Capture "script_hash" ScriptHash
        :> "json"
        :> Get '[JSON] ScriptJSON
      , _getScriptCBOR
        :: route
        :- Summary "Script CBOR"
        :> Description "CBOR representation of a `plutus` script"
        :> Capture "script_hash" ScriptHash
        :> "cbor"
        :> Get '[JSON] ScriptCBOR
    } deriving (Generic)
