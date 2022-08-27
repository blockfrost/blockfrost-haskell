{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Addresses
  where

import Data.Aeson (decode, eitherDecode, encode, object, (.=))
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_sample :: Spec
spec_sample = do
  it "parses address info sample" $ do
    eitherDecode addressInfoSample
    `shouldBe`
    Right addressInfoExpected

  it "parses address info sample" $ do
    eitherDecode addressInfoSample
    `shouldBe`
    Right addressInfoExpected

  it "parses address details sample" $ do
    eitherDecode addressDetailsSample
    `shouldBe`
    Right addressDetailsExpected

  it "parses address utxos sample" $ do
    eitherDecode addressUtxosSample
    `shouldBe`
    Right addressUtxosExpected

  it "parses address transactions sample" $ do
    eitherDecode addressTransactionsSample
    `shouldBe`
    Right addressTransactionsExpected

addressInfoSample = [r|
{
  "address": "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz",
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
  "stake_address": "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7",
  "type": "shelley",
  "script": false
}
|]

addressInfoExpected =
  AddressInfo
    { _addressInfoAddress = "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
    , _addressInfoAmount =
      [ AdaAmount 42000000
      , AssetAmount
          $ Money.mkSomeDiscrete
              "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
              unitScale
              12
      ]
    , _addressInfoStakeAddress = pure "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7"
    , _addressInfoType = Shelley
    , _addressInfoScript = False
    }

addressDetailsSample = [r|
{
  "address": "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz",
  "received_sum": [
    {
      "unit": "lovelace",
      "quantity": "42000000"
    },
    {
      "unit": "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e",
      "quantity": "12"
    }
  ],
  "sent_sum": [
    {
      "unit": "lovelace",
      "quantity": "42000000"
    },
    {
      "unit": "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e",
      "quantity": "12"
    }
  ],
  "tx_count": 12
}
|]

addressDetailsExpected =
    AddressDetails
    { _addressDetailsAddress = "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
    , _addressDetailsReceivedSum = amounts
    , _addressDetailsSentSum = amounts
    , _addressDetailsTxCount = 12
    }
    where amounts =
            [ AdaAmount 42000000
            , AssetAmount
                $ Money.mkSomeDiscrete
                        "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                        unitScale
                        12
            ]


addressUtxosSample = [r|
[
    {
        "tx_hash": "39a7a284c2a0948189dc45dec670211cd4d72f7b66c5726c08d9b3df11e44d58",
        "output_index": 0,
        "amount":
        [
            {
                "unit": "lovelace",
                "quantity": "42000000"
            }
        ],
        "block": "7eb8e27d18686c7db9a18f8bbcfe34e3fed6e047afaa2d969904d15e934847e6",
        "data_hash": "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710",
        "inline_datum": null,
        "reference_script_hash": null
    },
    {
        "tx_hash": "4c4e67bafa15e742c13c592b65c8f74c769cd7d9af04c848099672d1ba391b49",
        "output_index": 0,
        "amount":
        [
            {
                "unit": "lovelace",
                "quantity": "729235000"
            }
        ],
        "block": "953f1b80eb7c11a7ffcd67cbd4fde66e824a451aca5a4065725e5174b81685b7",
        "data_hash": null,
        "inline_datum": null,
        "reference_script_hash": null
    },
    {
        "tx_hash": "768c63e27a1c816a83dc7b07e78af673b2400de8849ea7e7b734ae1333d100d2",
        "output_index": 1,
        "amount":
        [
            {
                "unit": "lovelace",
                "quantity": "42000000"
            },
            {
                "unit": "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e",
                "quantity": "12"
            }
        ],
        "block": "5c571f83fe6c784d3fbc223792627ccf0eea96773100f9aedecf8b1eda4544d7",
        "data_hash": null,
        "inline_datum": null,
        "reference_script_hash": null
    }
]
|]

addressUtxosExpected =
  [ AddressUtxo
    { _addressUtxoTxHash = "39a7a284c2a0948189dc45dec670211cd4d72f7b66c5726c08d9b3df11e44d58"
    , _addressUtxoOutputIndex = 0
    , _addressUtxoAmount = [ AdaAmount 42000000 ]
    , _addressUtxoBlock = "7eb8e27d18686c7db9a18f8bbcfe34e3fed6e047afaa2d969904d15e934847e6"
    , _addressUtxoDataHash = Just "9e478573ab81ea7a8e31891ce0648b81229f408d596a3483e6f4f9b92d3cf710"
    , _addressUtxoInlineDatum = Nothing
    , _addressUtxoReferenceScriptHash = Nothing
    }
  , AddressUtxo
    { _addressUtxoTxHash = "4c4e67bafa15e742c13c592b65c8f74c769cd7d9af04c848099672d1ba391b49"
    , _addressUtxoOutputIndex = 0
    , _addressUtxoAmount = [ AdaAmount 729235000 ]
    , _addressUtxoBlock = "953f1b80eb7c11a7ffcd67cbd4fde66e824a451aca5a4065725e5174b81685b7"
    , _addressUtxoDataHash = Nothing
    , _addressUtxoInlineDatum = Nothing
    , _addressUtxoReferenceScriptHash = Nothing
    }
  , AddressUtxo
    { _addressUtxoTxHash = "768c63e27a1c816a83dc7b07e78af673b2400de8849ea7e7b734ae1333d100d2"
    , _addressUtxoOutputIndex = 1
    , _addressUtxoAmount =
        [ AdaAmount 42000000
        , AssetAmount
            $ Money.mkSomeDiscrete
                "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                 unitScale
                 12
        ]
    , _addressUtxoBlock = "5c571f83fe6c784d3fbc223792627ccf0eea96773100f9aedecf8b1eda4544d7"
    , _addressUtxoDataHash = Nothing
    , _addressUtxoInlineDatum = Nothing
    , _addressUtxoReferenceScriptHash = Nothing
    }
  ]

addressTransactionsSample = [r|
[
    {
        "tx_hash": "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b",
        "tx_index": 6,
        "block_height": 69,
        "block_time": 1635505891
    },
    {
        "tx_hash": "52e748c4dec58b687b90b0b40d383b9fe1f24c1a833b7395cdf07dd67859f46f",
        "tx_index": 9,
        "block_height": 4547,
        "block_time": 1635505987
    },
    {
        "tx_hash": "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b",
        "tx_index": 0,
        "block_height": 564654,
        "block_time": 1834505492
    }
]
|]

addressTransactionsExpected =
  [ AddressTransaction
    { _addressTransactionTxHash = "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b"
    , _addressTransactionTxIndex = 6
    , _addressTransactionBlockHeight = 69
    , _addressTransactionBlockTime = 1635505891
    }
  , AddressTransaction
    { _addressTransactionTxHash = "52e748c4dec58b687b90b0b40d383b9fe1f24c1a833b7395cdf07dd67859f46f"
    , _addressTransactionTxIndex = 9
    , _addressTransactionBlockHeight = 4547
    , _addressTransactionBlockTime = 1635505987
    }
  , AddressTransaction
    { _addressTransactionTxHash = "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b"
    , _addressTransactionTxIndex = 0
    , _addressTransactionBlockHeight = 564654
    , _addressTransactionBlockTime = 1834505492
    }
  ]
