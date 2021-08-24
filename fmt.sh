#!/usr/bin/env bash

go () { echo -n "> $1 "; pushd $1; stylish-haskell -r -v -i .; popd; }

go blockfrost-api
go blockfrost-client
go blockfrost-client-core
go blockfrost-pretty
