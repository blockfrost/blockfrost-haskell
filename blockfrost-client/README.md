# blockfrost-client

Haskell SDK for [Blockfrost.io](https://blockfrost.io) API.

## Development shell

```sh
nix-shell # from repository root, optional
cabal repl blockfrost-client
```

## Usage

[Blockfrost.io](https://blockfrost.io) API token is required to use this SDK.

### Obtaining token

Create a project at [blockfrost.io](https://blockfrost.io) and save the
generated token to a file.

For authentication, client uses a `Project` type which carries an environment and a token itself, for example `Project Testnet "someToken"`.

#### Project from file

Instead of constructing this type manually, you can load the `Project`
from file using `projectFromFile "/secrets/blockfrost-testnet-token"`.

In this case, token needs to be prefixed with the enviromnent to which
it belongs to. The format expected is `<env><token>` (similar to human readable
identifiers used for Cardano keys and addresses), examples:

* `testnet123notArealToken`
* `mainnet456notRealEither`
* `ipfs789mightBeARealOne`

### Using environment variables

In similar manner to loading project from file, you can use
`projectFromEnv` function to load the `Project` from file
pointed to by `BLOCKFROST_TOKEN_PATH` environment variable.

For custom variable name, a principled version `projectFromEnv' "CUSTOM_ENV_VAR"` can be used instead.

## Exploring the API

You can now perform queries from `cabal repl blockfrost-client`, an example session follows:

```haskell
λ: testnet <- projectFromFile "/secrets/blockfrost.testnet.token"
λ: testnet
Project {projectEnv = Testnet, projectId = "censored"}
λ: runBlockfrost testnet $ getLatestBlock
Right (Block {_blockTime = 1629716610s, ... })
λ: runBlockfrost testnet $ listPools
Right [PoolId "pool1adur9jcn0dkjpm3v8ayf94yn3fe5xfk2rqfz7rfpuh6cw6evd7w", ... ]
```

## Pagination and sorting

Some API functions have a variant with trailing `'`. These accept
`Paged` and `SortOrder` arguments for querying multiple pages
and setting a custom sort order.

For example to query the second page of pool list, we can use `listPools'`

```haskell
λ: runBlockfrost testnet $ listPools' (page 2) desc
```

To use default ordering (which is `asc` or `Ascending`), you can just use re-exported `def` from [Data.Default](https://hackage.haskell.org/package/data-default/docs/Data-Default.html).

```haskell
λ: runBlockfrost testnet $ listPools' (page 2) def
```

Similar, to just change an ordering but use default page size and a first page:

```haskell
λ: runBlockfrost testnet $ listPools' def desc
```

## Catching errors

We can use `tryError` inside `BlockfrostClient` monad to try a call,
when we it may fail (dealing with external inputs, enumerating addresses).

Complete program example with error handling:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  where

import Blockfrost.Client

main = do
  -- reads token from BLOCKFROST_TOKEN_PATH
  -- environment variable. It expects token
  -- prefixed with Blockfrost environment name
  -- e.g.: testnet-someTokenHash
  prj <- projectFromEnv
  res <- runBlockfrost prj $ do
    latestBlocks <- getLatestBlock
    (ers :: Either BlockfrostError [AccountReward]) <-
        tryError $ getAccountRewards "gonnaFail"
    pure (latestBlocks, ers)

  print res
```
