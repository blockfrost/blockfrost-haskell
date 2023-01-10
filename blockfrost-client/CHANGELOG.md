# Version [0.7.1.1](https://github.com/blockfrost/blockfrost-haskell/compare/v0.7.1.0...client-0.7.1.1) (2023-01-10)

* Allow blockfrost-api `0.8`

# Version [0.7.1.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.7.0.0...v0.7.1.0) (2023-01-03)

* Additions
  * `getNetworkEras` query for `/network/eras` endpoint [#33](https://github.com/blockfrost/blockfrost-haskell/pull/33/)

# Version [0.7.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.6.0.0...v0.7.0.0) (2022-10-11)

* Changes
  * `listScript` and `listScripts'` now return `ScriptHashList` newtype
    instead of `[ScriptHash]` due to overlapping instance conflicts

# Version [0.6.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.5.0.0...v0.6.0.0) (2022-08-31)

* Additions
  * `getScriptDatumCBOR` query for `/scripts/datum/{datum-hash}/cbor` endpoint

# Version [0.5.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.4.0.1...v0.5.0.0) (2022-06-06)

* Fix return types of `getEpochStakeByPool` and `getEpochStakeByPool'`
  from `StakeDistribution` to `PoolStakeDistribution`

# Version [0.4.0.1](https://github.com/blockfrost/blockfrost-haskell/compare/v0.4.0.0...v0.4.0.1) (2022-04-05)

* Allow servant `0.19`

# Version [0.4.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.3.1.0...v0.4.0.0) (2022-03-09)

* Build with `blockfrost-api 0.4`

# Version [0.3.1.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.3.0.0...v0.3.1.0) (2022-02-17)

* Additions
  * `getBlockAffectedAddresses` for `/blocks/${hash_or_number}/addresses` endpoint (Affected addresses in a block)

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

