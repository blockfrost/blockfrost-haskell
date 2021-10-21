# Version [0.2.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/0.1.0.0...0.2.0.0) (2021-10-14)

* Breaking changes
  * `AddressUTXO` renamed to `AddressUtxo` along with its fields
  * `ProtocolParams` `collateralPercent` field type changed from `Double` to `Integer`

* Alonzo additions
  * `collateral` field to `UtxoInput`
  * `redeemerCount` field to `Transaction`
  * `dataHash` field to `UtxoInput` and `UtxoOutput`
  * `dataHash` field to `AdressUTXO`
  * `outputIndex` field to `UtxoOutput`
  * `script` field to `AddressInfo`
  * `NetworkSupply`
    * `locked` field representing supply locked by all scripts
    * `treasury` field (current treasury supply)
    * `reserves` field (current reserves supply)
  * `/txs/${hash}/redeemers` endpoint with `TransactionRedeemer` and `ValidationPurpose` types
  * Epoch cost model parameters to `ProtocolParams`
  * `/scripts` endpoint and `ScriptHash` newtype
  * `/scripts/${script_hash}` endpoint with `Script` data type
  * `/scripts/${script_hash}/redeemers` endpoint with `ScriptRedeemer` data type

* Other
 * Added `liveDelegators` field to `PoolInfo`

# Version [0.1.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/initial...0.1.0.0) (2021-09-14)

* Initial release

---

`blockfrost-api` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

