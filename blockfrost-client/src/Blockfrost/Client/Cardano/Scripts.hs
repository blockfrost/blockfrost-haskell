-- | Script queries

module Blockfrost.Client.Cardano.Scripts
  ( listScripts
  , listScripts'
  , getScript
  , getScriptRedeemers
  , getScriptRedeemers'
  , getScriptDatum
  , getScriptJSON
  , getScriptCBOR
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

scriptsClient :: Project -> ScriptsAPI (AsClientT BlockfrostClient)
scriptsClient = fromServant . _scripts . cardanoClient

listScripts_ :: Project -> Paged -> SortOrder -> BlockfrostClient [ScriptHash]
listScripts_ = _listScripts . scriptsClient

-- | List scripts
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
listScripts' :: Paged -> SortOrder -> BlockfrostClient [ScriptHash]
listScripts' pg s = go (\p -> listScripts_ p pg s)

-- | List scripts
listScripts :: BlockfrostClient [ScriptHash]
listScripts = listScripts' def def

getScript_ :: Project -> ScriptHash -> BlockfrostClient Script
getScript_ = _getScript . scriptsClient

-- | Get specific script information
getScript:: ScriptHash -> BlockfrostClient Script
getScript sh = go (`getScript_` sh)

getScriptRedeemers_ :: Project -> ScriptHash -> Paged -> SortOrder -> BlockfrostClient [ScriptRedeemer]
getScriptRedeemers_ = _getScriptRedeemers . scriptsClient

-- | Get redeemers of a specific script
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getScriptRedeemers' :: ScriptHash -> Paged -> SortOrder -> BlockfrostClient [ScriptRedeemer]
getScriptRedeemers' sh pg s = go (\p -> getScriptRedeemers_ p sh pg s)

-- | Get redeemers of a specific script
getScriptRedeemers :: ScriptHash -> BlockfrostClient [ScriptRedeemer]
getScriptRedeemers sh = getScriptRedeemers' sh def def

getScriptDatum_ :: Project -> DatumHash -> BlockfrostClient ScriptDatum
getScriptDatum_ = _getScriptDatum . scriptsClient

-- | Get specific datum
getScriptDatum :: DatumHash -> BlockfrostClient ScriptDatum
getScriptDatum sh = go (`getScriptDatum_` sh)

getScriptJSON_ :: Project -> ScriptHash -> BlockfrostClient ScriptJSON
getScriptJSON_ = _getScriptJSON . scriptsClient

-- | Get a JSON representation of a `timelock` script
getScriptJSON :: ScriptHash -> BlockfrostClient ScriptJSON
getScriptJSON sh = go (`getScriptJSON_` sh)

getScriptCBOR_ :: Project -> ScriptHash -> BlockfrostClient ScriptCBOR
getScriptCBOR_ = _getScriptCBOR . scriptsClient

-- | Get a CBOR representation of a `plutus` script
getScriptCBOR :: ScriptHash -> BlockfrostClient ScriptCBOR
getScriptCBOR sh = go (`getScriptCBOR_` sh)
