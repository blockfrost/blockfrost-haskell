<img src="https://blockfrost.io/images/logo.svg" width="250" align="right" height="90">

# blockfrost-haskell

<br/>

<p align="center">Haskell SDK for <a href="https://blockfrost.io">Blockfrost.io</a> API.</p>
<p align="center">
  <a href="#about">About</a> •
  <a href="#getting-started">Getting started</a> •
  <a href="#installation">Installation</a> •
  <a href="#usage">Usage</a>
</p>

<br/>

## About

The repository provides an API definition, data types, client and utilities
for working with Blockfrost. We are striving to provide begginer-friendly
interface while adding a bit of type safety, especially when working with monetary
values.

### Packages

* [`blockfrost-api`](./blockfrost-api/#readme)
  Types, sample data, API definition and tests. See the [`README.md`](./blockfrost-api/#readme)
  for quick tutorial
* [`blockfrost-client-core`](./blockfrost-client-core/#readme)
  Instances and helpers share by all clients.
* [`blockfrost-client`](./blockfrost-client/#readme)
  Blockfrost client for use with `mtl`. Its [`README.md`](./blockfrost-api/#readme)
  contains an introduction and usage examples.
* [`blockfrost-pretty`](./blockfrost-pretty/#readme)
  Pretty printing utilities for pretty printing Ada values
  and various Blockfrost types.

## Getting started

To use this SDK, you first need to log in to [blockfrost.io](https://blockfrost.io), create your project and retrieve the API token.

<img src="https://i.imgur.com/smY12ro.png">

<br/>

## Installation

The packages are not yet published on Hackage. For now, you can
either work within this repository using plain `cabal` or in combination
with `nix`.

### `cabal`

If you already have `GHC` and `cabal` installed:

```sh
git clone https://github.com/blockfrost/blockfrost-haskell
cd blockfrost-haskell
cabal build all
cabal repl blockfrost-client
```

Note: Due to TLS support, you might need to
provide `zlib` headers if compilation
of `http-client-tls` fails. (On NixOS this is `nix-shell -p zlib.dev`).

### `nix`

Using `nix-shell`, you can obtain a preconfigured environment
with `GHC` and `cabal`:

```sh
git clone https://github.com/blockfrost/blockfrost-haskell
cd blockfrost-haskell
nix-shell
cabal build all
cabal repl blockfrost-client
```

## Usage

See [blockfrost-client](./blockfrost-client/#readme) for a tutorial
and usage examples.

Readme of [blockfrost-api](./blockfrost-api/#readme)
contains a short primer for working with Blockfrost
types, data samples and monetary values.
