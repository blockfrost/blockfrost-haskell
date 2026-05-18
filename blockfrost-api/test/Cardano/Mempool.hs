{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Mempool
  where

import Data.Aeson (decode, eitherDecode, encode, object, (.=), Value (Array))
import qualified Data.Either
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_mempool :: Spec
spec_mempool = do
  it "parses mempool transaction sample" $ do
    eitherDecode mempoolTransactionInput
    `shouldBe`
    Right mempoolTransactionSample

mempoolTransactionInput = [r|
{
  "tx": {
    "hash": "1e043f100dce12d107f679685acd2fc0610e10f72a92d412794c9773d11d8477",
    "output_amount": [
      {
        "unit": "lovelace",
        "quantity": "42000000"
      }
    ],
    "fees": "182485",
    "deposit": "0",
    "size": 433,
    "invalid_before": null,
    "invalid_hereafter": "13885913",
    "utxo_count": 4,
    "withdrawal_count": 0,
    "mir_cert_count": 0,
    "delegation_count": 0,
    "stake_cert_count": 0,
    "pool_update_count": 0,
    "pool_retire_count": 0,
    "asset_mint_or_burn_count": 0,
    "redeemer_count": 0,
    "valid_contract": true
  },
  "inputs": [
    {
      "address": "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv",
      "tx_hash": "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0",
      "output_index": 0,
      "collateral": false,
      "reference": false
    }
  ],
  "outputs": [
    {
      "address": "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv",
      "amount": [
        {
          "unit": "lovelace",
          "quantity": "42000000"
        },
        {
          "unit": "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e",
          "quantity": "12"
        }
      ],
      "output_index": 0,
      "data_hash": "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710",
      "inline_datum": "19a6aa",
      "collateral": false,
      "reference_script_hash": "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1"
    }
  ],
  "redeemers": [
    {
      "tx_index": 0,
      "purpose": "spend",
      "unit_mem": "1700",
      "unit_steps": "476468"
    }
  ]
}
|]
