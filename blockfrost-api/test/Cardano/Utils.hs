{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Utils
  where

import Data.Aeson (decode, eitherDecode, encode, object, (.=), Value (Array))
import qualified Data.Either
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_scripts :: Spec
spec_scripts = do
  it "parses derived address sample" $ do
    eitherDecode derivedAddressSample
    `shouldBe`
    Right derivedAddressExpected

  it "parses tx eval sample" $ do
    eitherDecode txEvalSample
    `shouldBe`
    Right txEvalExpected

  it "fails to parse tx eval error" $ do
    eitherDecode txEvalErrorSample
    `shouldSatisfy`
    (Data.Either.isLeft :: Either String TxEval -> Bool)

  it "parses tx eval input sample" $ do
    eitherDecode txEvalInputSample
    `shouldBe`
    Right txEvalInputExpected

derivedAddressSample = [r|
{
  "xpub": "d507c8f866691bd96e131334c355188b1a1d0b2fa0ab11545075aab332d77d9eb19657ad13ee581b56b0f8d744d66ca356b93d42fe176b3de007d53e9c4c4e7a",
  "role": 0,
  "index": 0,
  "address": "addr1q90sqnljxky88s0jsnps48jd872p7znzwym0jpzqnax6qs5nfrlkaatu28n0qzmqh7f2cpksxhpc9jefx3wrl0a2wu8q5amen7"
}
|]

derivedAddressExpected =
  DerivedAddress
    { _derivedAddressXpub    = "d507c8f866691bd96e131334c355188b1a1d0b2fa0ab11545075aab332d77d9eb19657ad13ee581b56b0f8d744d66ca356b93d42fe176b3de007d53e9c4c4e7a"
    , _derivedAddressRole    = 0
    , _derivedAddressIndex   = 0
    , _derivedAddressAddress = "addr1q90sqnljxky88s0jsnps48jd872p7znzwym0jpzqnax6qs5nfrlkaatu28n0qzmqh7f2cpksxhpc9jefx3wrl0a2wu8q5amen7"
    }

txEvalSample = [r|
{
  "jsonrpc": "2.0",
  "method": "evaluateTransaction",
  "result": [{
    "validator": "spend:0",
    "budget": {
      "memory": 1700,
      "cpu": 476468
    }
  }]
}
|]

txEvalExpected = evalSample

-- Stolen from
-- https://github.com/CardanoSolutions/ogmios/blob/master/server/test/vectors/EvaluateTransactionResponse/000.json
-- Mozilla Public License 2.0
txEvalErrorSample = [r|
{
  "jsonrpc": "2.0",
  "method": "evaluateTransaction",
  "error": {
    "code": 3010,
    "message": "Some scripts of the transactions terminated with error(s).",
    "data": [
      {
        "validator": "spend:4",
        "error": {
          "code": 3011,
          "message": "An associated script witness is missing. Indeed, any script used in a transaction (when spending, minting, withdrawing or publishing certificates) must be provided in full with the transaction. Scripts must therefore be added either to the witness set or provided as a reference inputs should you use Plutus V2+ and a format from Babbage and beyond.",
          "data": {
            "missingScripts": [
              "certificate:3"
            ]
          }
        }
      },
      {
        "validator": "mint:0",
        "error": {
          "code": 3011,
          "message": "An associated script witness is missing. Indeed, any script used in a transaction (when spending, minting, withdrawing or publishing certificates) must be provided in full with the transaction. Scripts must therefore be added either to the witness set or provided as a reference inputs should you use Plutus V2+ and a format from Babbage and beyond.",
          "data": {
            "missingScripts": [
              "certificate:11"
            ]
          }
        }
      },
      {
        "validator": "mint:3",
        "error": {
          "code": 3011,
          "message": "An associated script witness is missing. Indeed, any script used in a transaction (when spending, minting, withdrawing or publishing certificates) must be provided in full with the transaction. Scripts must therefore be added either to the witness set or provided as a reference inputs should you use Plutus V2+ and a format from Babbage and beyond.",
          "data": {
            "missingScripts": [
              "withdrawal:7"
            ]
          }
        }
      },
      {
        "validator": "mint:7",
        "error": {
          "code": 3011,
          "message": "An associated script witness is missing. Indeed, any script used in a transaction (when spending, minting, withdrawing or publishing certificates) must be provided in full with the transaction. Scripts must therefore be added either to the witness set or provided as a reference inputs should you use Plutus V2+ and a format from Babbage and beyond.",
          "data": {
            "missingScripts": [
              "mint:4"
            ]
          }
        }
      },
      {
        "validator": "mint:11",
        "error": {
          "code": 3117,
          "message": "The transaction contains unknown UTxO references as inputs. This can happen if the inputs you're trying to spend have already been spent, or if you've simply referred to non-existing UTxO altogether. The field 'data.unknownOutputReferences' indicates all unknown inputs.",
          "data": {
            "unknownOutputReferences": [
              {
                "transaction": {
                  "id": "a10897006ca78f6ce87fc6e2b139d92a896de01d62fe01f4fd0eccc6a10075c1"
                },
                "index": 14
              }
            ]
          }
        }
      },
      {
        "validator": "certificate:2",
        "error": {
          "code": 3011,
          "message": "An associated script witness is missing. Indeed, any script used in a transaction (when spending, minting, withdrawing or publishing certificates) must be provided in full with the transaction. Scripts must therefore be added either to the witness set or provided as a reference inputs should you use Plutus V2+ and a format from Babbage and beyond.",
          "data": {
            "missingScripts": [
              "withdrawal:2"
            ]
          }
        }
      }
    ]
  },
  "id": null
}
|]

txEvalInputSample = [r|
{
  "cbor": "sample",
  "additionalUtxoSet": []
}
|]

txEvalInputExpected =
  TxEvalInput
    { _txEvalInputCbor              = CBORString "sample"
    , _txEvalInputAdditionalUtxoSet = Array mempty
    }
