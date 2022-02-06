# Version [0.3.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.2.1.0...v0.3.0.0) (2022-02-07)

* Changes
  * `UtxoInput` and `UtxoOutput` `dataHash` fields changed from `Maybe Text` to `Maybe DataHash`

* Additions

# Version [0.2.1.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.2.0.0...v0.2.1.0) (2021-11-15)

* Additions
  * `ToSample` instances for `ScriptDatum`, `ScriptJSON`, `ScriptCBOR`
  * `/accounts/{stake_address}/addresses` endpoint
  * `/accounts/{stake_address}/addresses/assets` endpoint

# Version [0.2.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.1.0.0...v0.2.0.0) (2021-10-29)

* Breaking changes
  * `AddressUTXO` renamed to `AddressUtxo` along with its fields
  * `ProtocolParams` `collateralPercent` field type changed from `Double` to `Integer`
  * `TransactionMetaCBOR` now uses `metadata` field instead of `CBORMetadata` which holds the same CBOR value as before but without `\\x` prefix

* Alonzo additions
  * `collateral` field to `UtxoInput`
  * `redeemerCount` field to `Transaction`
  * `dataHash` field to `UtxoInput` and `UtxoOutput`
  * `dataHash` field to `AdressUTXO`
  * `outputIndex` field to `UtxoOutput`
  * `script` field to `AddressInfo`
  * `datumHash` field to `ScriptRedeemer`
  * `scriptHash` and `datumHash` field to `TransactionRedeemer`
  * `NetworkSupply`
    * `locked` field representing supply locked by all scripts
    * `treasury` field (current treasury supply)
    * `reserves` field (current reserves supply)
  * `/txs/${hash}/redeemers` endpoint with `TransactionRedeemer` and `ValidationPurpose` types
  * Epoch cost model parameters to `ProtocolParams`
  * `/scripts` endpoint and `ScriptHash` newtype
  * `/scripts/${script_hash}` endpoint with `Script` data type
  * `/scripts/${script_hash}/redeemers` endpoint with `ScriptRedeemer` data type
  * `/scripts/datum/${datum_hash}` endpoint with `ScriptDatum` data type
  * `/scripts/${script_hash}/json` endpoint with `ScriptJSON` data type
  * `/scripts/${script_hash}/cbor` endpoint with `ScriptCBOR` data type

* Other
 * Added `liveDelegators` field to `PoolInfo`

# Version [0.1.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/initial...v0.1.0.0) (2021-09-14)

* Initial release

---

`blockfrost-api` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

