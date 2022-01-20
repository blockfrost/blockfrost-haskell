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

scriptsClient :: MonadBlockfrost m => Project -> ScriptsAPI (AsClientT m)
scriptsClient = fromServant . _scripts . cardanoClient

listScripts_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [ScriptHash]
listScripts_ = _listScripts . scriptsClient

-- | List scripts
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
listScripts' :: MonadBlockfrost m => Paged -> SortOrder -> m [ScriptHash]
listScripts' pg s = go (\p -> listScripts_ p pg s)

-- | List scripts
listScripts :: MonadBlockfrost m => m [ScriptHash]
listScripts = listScripts' def def

getScript_ :: MonadBlockfrost m => Project -> ScriptHash -> m Script
getScript_ = _getScript . scriptsClient

-- | Get specific script information
getScript :: MonadBlockfrost m => ScriptHash -> m Script
getScript sh = go (`getScript_` sh)

getScriptRedeemers_ :: MonadBlockfrost m => Project -> ScriptHash -> Paged -> SortOrder -> m [ScriptRedeemer]
getScriptRedeemers_ = _getScriptRedeemers . scriptsClient

-- | Get redeemers of a specific script
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getScriptRedeemers' :: MonadBlockfrost m => ScriptHash -> Paged -> SortOrder -> m [ScriptRedeemer]
getScriptRedeemers' sh pg s = go (\p -> getScriptRedeemers_ p sh pg s)

-- | Get redeemers of a specific script
getScriptRedeemers :: MonadBlockfrost m => ScriptHash -> m [ScriptRedeemer]
getScriptRedeemers sh = getScriptRedeemers' sh def def

getScriptDatum_ :: MonadBlockfrost m => Project -> DatumHash -> m ScriptDatum
getScriptDatum_ = _getScriptDatum . scriptsClient

-- | Get specific datum
getScriptDatum :: MonadBlockfrost m => DatumHash -> m ScriptDatum
getScriptDatum sh = go (`getScriptDatum_` sh)

getScriptJSON_ :: MonadBlockfrost m => Project -> ScriptHash -> m ScriptJSON
getScriptJSON_ = _getScriptJSON . scriptsClient

-- | Get a JSON representation of a `timelock` script
getScriptJSON :: MonadBlockfrost m => ScriptHash -> m ScriptJSON
getScriptJSON sh = go (`getScriptJSON_` sh)

getScriptCBOR_ :: MonadBlockfrost m => Project -> ScriptHash -> m ScriptCBOR
getScriptCBOR_ = _getScriptCBOR . scriptsClient

-- | Get a CBOR representation of a `plutus` script
getScriptCBOR :: MonadBlockfrost m => ScriptHash -> m ScriptCBOR
getScriptCBOR sh = go (`getScriptCBOR_` sh)
