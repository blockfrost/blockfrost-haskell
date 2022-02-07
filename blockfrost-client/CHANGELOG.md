# Version [0.3.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.2.1.0...v0.3.0.0) (2022-02-07)

* Changes
  * Client functions switched to tagless variants using `MonadBlockfrost` allowing to use custom base monad
    for client functions.  Provided instances for `BlockfrostClient`, `IO` and `ClientM`.

* Additions
  * Export `getTxRedeemers` from `Blockfrost.Client`

# Version [0.2.1.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.2.0.0...v0.2.1.0) (2021-11-15)

* Additions
  * `getAccountAssociatedAddresses` query for `/accounts/{stake_address}/addresses` endpoint
  * `getAccountAssociatedAssets` query for `/accounts/{stake_address}/addresses/assets` endpoint

# Version [0.2.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.1.0.0...v0.2.0.0) (2021-10-29)

* Alonzo additions
  * `getTxRedeemers` for `/txs/${hash}/redeemers` endpoint
  * `listScripts` for `/scripts` endpoint, principled `listScripts'`
  * `getScript` for `/scripts/${script_hash}` endpoint
  * `getScriptRedeemers` for `/scripts/${script_hash}/redeemers` endpoint, principled `getScriptRedeemers'`
  * `getAddressUtxosAsset` for `/addresses/${address}/utxos/${asset}`, principled `getAddressUtxosAsset'`
  * `getScriptDatum` for `/scripts/datum/${datum_hash}` endpoint
  * `getScriptJSON` for `/scripts/${script_hash}/json` endpoint
  * `getScriptCBOR` for `/scripts/${script_hash}/cbor` endpoint

# Version [0.1.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/initial...v0.1.0.0) (2021-09-14)

* Added `allPages`, re-exported couple more pagination helpers
* Initial release

---

`blockfrost-client` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

