# Version [0.6.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.5.0.0...v0.6.0.0) (2022-08-31)

* Additions
  * HTTP Error `425`
    * `BlockfrostMempoolFullOrPinQueueFull` to `BlockfrostError` 
      * related to `/tx/submit` endpoint
      * or to `IPFS` pinning `/ipfs/pin/add/{IPFS_path}` endpoint
  * `Preprod` and `Preview` environments

* Changes
  * Order of results returned by `allPages` is now the same as returned by the API. Was unintentionally reversed before.

# Version [0.4.0.2](https://github.com/blockfrost/blockfrost-haskell/compare/v0.4.0.1...client-core-0.4.0.2) (2022-06-06)

* Allow blockfrost-api `0.5`

# Version [0.4.0.1](https://github.com/blockfrost/blockfrost-haskell/compare/v0.4.0.0...v0.4.0.1) (2022-04-05)

* Allow servant `0.19`

# Version [0.4.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.3.1.0...v0.4.0.0) (2022-03-09)

* Re-export `nextPage`

# Version [0.2.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/v0.1.0.0...v0.2.0.0) (2021-10-29)

* Dropped `Alonzo` environment
* Switched category to `Cardano`

# Version [0.1.0.0](https://github.com/blockfrost/blockfrost-haskell/compare/initial...v0.1.0.0) (2021-09-14)

* Initial release

---

`blockfrost-client-core` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

