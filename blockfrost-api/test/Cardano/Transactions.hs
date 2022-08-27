{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Transactions
  where

import Data.Aeson (decode, eitherDecode, encode, object, (.=))
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_txs :: Spec
spec_txs = do
  it "parses transaction sample" $ do
    eitherDecode transactionSample
    `shouldBe`
    Right transactionExpected

  it "parses transaction utxos sample" $ do
    eitherDecode transactionUtxosSample
    `shouldBe`
    Right transactionUtxosExpected

  it "parses transaction redeemers sample" $ do
    eitherDecode transactionRedeemerSample
    `shouldBe`
    Right transactionRedeemerExpected

  it "parses transaction stake sample" $ do
    eitherDecode transactionStakeSample
    `shouldBe`
    Right transactionStakeExpected

  it "parses transaction delegation sample" $ do
    eitherDecode transactionDelegationSample
    `shouldBe`
    Right transactionDelegationExpected

  it "parses transaction withdrawal sample" $ do
    eitherDecode transactionWithdrawalSample
    `shouldBe`
    Right transactionWithdrawalExpected

  it "parses transaction mir sample" $ do
    eitherDecode transactionMirSample
    `shouldBe`
    Right transactionMirExpected

  it "parses transaction pool update sample" $ do
    eitherDecode transactionPoolUpdateSample
    `shouldBe`
    Right transactionPoolUpdateExpected

  it "parses transaction pool retiring sample" $ do
    eitherDecode transactionPoolRetiringSample
    `shouldBe`
    Right transactionPoolRetiringExpected

  it "parses transaction meta (JSON) sample" $ do
    eitherDecode transactionMetaJSONSample
    `shouldBe`
    Right transactionMetaJSONExpected

  it "parses transaction meta (CBOR) sample" $ do
    eitherDecode transactionMetaCBORSample
    `shouldBe`
    Right transactionMetaCBORExpected

transactionSample = [r|
{
  "hash": "1e043f100dce12d107f679685acd2fc0610e10f72a92d412794c9773d11d8477",
  "block": "356b7d7dbb696ccd12775c016941057a9dc70898d87a63fc752271bb46856940",
  "block_height": 123456,
  "slot": 42000000,
  "index": 1,
  "output_amount": [
    {
      "unit": "lovelace",
      "quantity": "42000000"
    },
    {
      "unit": "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e",
      "quantity": "12"
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
}
|]

sampleAmounts :: [Amount]
sampleAmounts =
  [ AdaAmount 42000000
  , AssetAmount
      $ Money.mkSomeDiscrete
          "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
          unitScale
          12
  ]

transactionExpected =
  Transaction
    { _transactionHash = "1e043f100dce12d107f679685acd2fc0610e10f72a92d412794c9773d11d8477"
    , _transactionBlock = "356b7d7dbb696ccd12775c016941057a9dc70898d87a63fc752271bb46856940"
    , _transactionBlockHeight = 123456
    , _transactionSlot = 42000000
    , _transactionIndex = 1
    , _transactionOutputAmount = sampleAmounts
    , _transactionFees = 182485
    , _transactionDeposit = 0
    , _transactionSize = 433
    , _transactionInvalidBefore = Nothing
    , _transactionInvalidHereafter = Just "13885913"
    , _transactionUtxoCount = 4
    , _transactionWithdrawalCount = 0
    , _transactionMirCertCount =  0
    , _transactionDelegationCount = 0
    , _transactionStakeCertCount = 0
    , _transactionPoolUpdateCount = 0
    , _transactionPoolRetireCount = 0
    , _transactionAssetMintOrBurnCount = 0
    , _transactionRedeemerCount = 0
    , _transactionValidContract = True
    }

transactionUtxosSample = [r|
{
  "hash": "1e043f100dce12d107f679685acd2fc0610e10f72a92d412794c9773d11d8477",
  "inputs": [
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
      "tx_hash": "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0",
      "output_index": 0,
      "collateral": false,
      "data_hash": "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710",
      "inline_datum": null,
      "reference_script_hash": "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1",
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
      "data_hash": "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710",
      "output_index": 0,
      "collateral": false,
      "inline_datum": "19a6aa",
      "reference_script_hash": "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1"
    }
  ]
}
|]

utxoInSample :: UtxoInput
utxoInSample =
    UtxoInput
      { _utxoInputAddress = "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv"
      , _utxoInputAmount = sampleAmounts
      , _utxoInputTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0"
      , _utxoInputOutputIndex = 0
      , _utxoInputCollateral = False
      , _utxoInputDataHash = Just "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
      , _utxoInputInlineDatum = Nothing
      , _utxoInputReferenceScriptHash = Just "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1"
      , _utxoInputReference = False
      }

utxoOutSample :: UtxoOutput
utxoOutSample =
  UtxoOutput
    { _utxoOutputAddress = "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv"
    , _utxoOutputAmount = sampleAmounts
    , _utxoOutputDataHash = Just "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    , _utxoOutputOutputIndex = 0
    , _utxoOutputCollateral = False
    , _utxoOutputInlineDatum = Just $ InlineDatum $ ScriptDatumCBOR "19a6aa"
    , _utxoOutputReferenceScriptHash = Just "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1"
    }

transactionUtxosExpected =
  TransactionUtxos
    { _transactionUtxosHash = "1e043f100dce12d107f679685acd2fc0610e10f72a92d412794c9773d11d8477"
    , _transactionUtxosInputs = pure utxoInSample
    , _transactionUtxosOutputs = pure utxoOutSample
    }

transactionRedeemerSample = [r|
{
  "tx_index": 0,
  "purpose": "spend",
  "script_hash": "ec26b89af41bef0f7585353831cb5da42b5b37185e0c8a526143b824",
  "redeemer_data_hash": "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
  "datum_hash": "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
  "unit_mem": "1700",
  "unit_steps": "476468",
  "fee": "172033"
}
|]

transactionRedeemerExpected =
  TransactionRedeemer
    { _transactionRedeemerTxIndex = 0
    , _transactionRedeemerPurpose = Spend
    , _transactionRedeemerScriptHash = "ec26b89af41bef0f7585353831cb5da42b5b37185e0c8a526143b824"
    , _transactionRedeemerRedeemerDataHash = "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
    -- deprecated
    , _transactionRedeemerDatumHash = "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
    , _transactionRedeemerUnitMem = 1700
    , _transactionRedeemerUnitSteps = 476468
    , _transactionRedeemerFee = 172033
    }

transactionStakeSample = [r|
{
  "cert_index": 0,
  "address": "stake1u9t3a0tcwune5xrnfjg4q7cpvjlgx9lcv0cuqf5mhfjwrvcwrulda",
  "registration": true
}
|]

transactionStakeExpected =
  TransactionStake
    { _transactionStakeCertIndex = 0
    , _transactionStakeAddress = "stake1u9t3a0tcwune5xrnfjg4q7cpvjlgx9lcv0cuqf5mhfjwrvcwrulda"
    , _transactionStakeRegistration = True
    }

transactionDelegationSample =  [r|
{
  "index": 0,
  "cert_index": 0,
  "address": "stake1u9r76ypf5fskppa0cmttas05cgcswrttn6jrq4yd7jpdnvc7gt0yc",
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "active_epoch": 210
}
|]

transactionDelegationExpected =
  TransactionDelegation
    { _transactionDelegationCertIndex = 0
    , _transactionDelegationAddress = "stake1u9r76ypf5fskppa0cmttas05cgcswrttn6jrq4yd7jpdnvc7gt0yc"
    , _transactionDelegationPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    , _transactionDelegationActiveEpoch = 210
    }

transactionWithdrawalSample = [r|
{
  "address": "stake1u9r76ypf5fskppa0cmttas05cgcswrttn6jrq4yd7jpdnvc7gt0yc",
  "amount": "431833601"
}
|]

transactionWithdrawalExpected =
  TransactionWithdrawal
    { _transactionWithdrawalAddress = "stake1u9r76ypf5fskppa0cmttas05cgcswrttn6jrq4yd7jpdnvc7gt0yc"
    , _transactionWithdrawalAmount = 431833601
    }

transactionMirSample = [r|
{
  "pot": "reserve",
  "cert_index": 0,
  "address": "stake1u9r76ypf5fskppa0cmttas05cgcswrttn6jrq4yd7jpdnvc7gt0yc",
  "amount": "431833601"
}
|]

transactionMirExpected =
  TransactionMir
    { _transactionMirPot = Reserve
    , _transactionMirCertIndex = 0
    , _transactionMirAddress = "stake1u9r76ypf5fskppa0cmttas05cgcswrttn6jrq4yd7jpdnvc7gt0yc"
    , _transactionMirAmount = 431833601
    }

transactionPoolUpdateSample = [r|
{
  "cert_index": 0,
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "vrf_key": "0b5245f9934ec2151116fb8ec00f35fd00e0aa3b075c4ed12cce440f999d8233",
  "pledge": "5000000000",
  "margin_cost": 0.05,
  "fixed_cost": "340000000",
  "reward_account": "stake1uxkptsa4lkr55jleztw43t37vgdn88l6ghclfwuxld2eykgpgvg3f",
  "owners": [
    "stake1u98nnlkvkk23vtvf9273uq7cph5ww6u2yq2389psuqet90sv4xv9v"
  ],
  "metadata": {
    "url": "https://stakenuts.com/mainnet.json",
    "hash": "47c0c68cb57f4a5b4a87bad896fc274678e7aea98e200fa14a1cb40c0cab1d8c",
    "ticker": "NUTS",
    "name": "Stake Nuts",
    "description": "The best pool ever",
    "homepage": "https://stakentus.com/"
  },
  "relays": [
    {
      "ipv4": "4.4.4.4",
      "ipv6": "https://stakenuts.com/mainnet.json",
      "dns": "relay1.stakenuts.com",
      "dns_srv": "_relays._tcp.relays.stakenuts.com",
      "port": 3001
    }
  ],
  "active_epoch": 210
}
|]

transactionPoolUpdateExpected =
  TransactionPoolUpdate
    { _transactionPoolUpdateCertIndex = 0
    , _transactionPoolUpdatePoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    , _transactionPoolUpdateVrfKey = "0b5245f9934ec2151116fb8ec00f35fd00e0aa3b075c4ed12cce440f999d8233"
    , _transactionPoolUpdatePledge = 5000000000
    , _transactionPoolUpdateMarginCost = 0.05
    , _transactionPoolUpdateFixedCost = 340000000
    , _transactionPoolUpdateRewardAccount = "stake1uxkptsa4lkr55jleztw43t37vgdn88l6ghclfwuxld2eykgpgvg3f"
    , _transactionPoolUpdateOwners = [ "stake1u98nnlkvkk23vtvf9273uq7cph5ww6u2yq2389psuqet90sv4xv9v" ]
    , _transactionPoolUpdateMetadata = Just samplePoolUpdateMetadata
    , _transactionPoolUpdateRelays = [ samplePoolRelay ]
    , _transactionPoolUpdateActiveEpoch = 210
    }

samplePoolUpdateMetadata :: PoolUpdateMetadata
samplePoolUpdateMetadata =
  PoolUpdateMetadata
    { _poolUpdateMetadataUrl = Just "https://stakenuts.com/mainnet.json"
    , _poolUpdateMetadataHash = Just "47c0c68cb57f4a5b4a87bad896fc274678e7aea98e200fa14a1cb40c0cab1d8c"
    , _poolUpdateMetadataTicker = Just "NUTS"
    , _poolUpdateMetadataName = Just "Stake Nuts"
    , _poolUpdateMetadataDescription = Just "The best pool ever"
    , _poolUpdateMetadataHomepage = Just "https://stakentus.com/"
    }

transactionPoolRetiringSample = [r|
{
  "cert_index": 0,
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "retiring_epoch": 216
}
|]

transactionPoolRetiringExpected =
  TransactionPoolRetiring
    { _transactionPoolRetiringCertIndex = 0
    , _transactionPoolRetiringPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    , _transactionPoolRetiringRetiringEpoch = 216
    }

transactionMetaJSONSample = [r|
[
  {
    "label": "1967",
    "json_metadata": {
      "metadata": "https://nut.link/metadata.json",
      "hash": "6bf124f217d0e5a0a8adb1dbd8540e1334280d49ab861127868339f43b3948af"
    }
  },
  {
    "label": "1968",
    "json_metadata": {
      "ADAUSD": [
        {
          "value": "0.15409850555139935",
          "source": "ergoOracles"
        }
      ]
    }
  }
]
|]

transactionMetaJSONExpected =
    let oracleMeta val =
          object [
            "ADAUSD" .=
              [ object [ "value" .= (val :: Text)
                       , "source" .= ("ergoOracles" :: Text) ]
              ]
          ]
    in
    [ TransactionMetaJSON
        "1967"
        (Just $ object
           [ "metadata" .= ("https://nut.link/metadata.json" :: Text)
           , "hash" .= ("6bf124f217d0e5a0a8adb1dbd8540e1334280d49ab861127868339f43b3948af" :: Text)
           ])
    , TransactionMetaJSON
        "1968"
        (Just $ oracleMeta "0.15409850555139935")
    ]

transactionMetaCBORSample = [r|
{
  "label": "1968",
  "metadata": "a100a16b436f6d62696e6174696f6e8601010101010c"
}
|]

transactionMetaCBORExpected =
    TransactionMetaCBOR
      "1968"
      (Just "a100a16b436f6d62696e6174696f6e8601010101010c")
