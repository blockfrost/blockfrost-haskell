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
    (Data.Either.isLeft . _txEvalResult . Data.Either.fromRight undefined)

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
  "type": "jsonwsp/response",
  "version": "1.0",
  "servicename": "ogmios",
  "methodname": "EvaluateTx",
  "result": {
    "EvaluationResult": {
      "spend:0": {
        "memory": 1765011,
        "steps": 503871230
      }
    }
  },
  "reflection": {
    "id": "3e7eace0-a3d2-4020-aecb-c5b6e7910568"
  }
}
|]

txEvalExpected = evalSample

-- Stolen from
-- https://github.com/CardanoSolutions/ogmios/blob/v5.6.0/server/test/vectors/TxSubmission/Response/EvaluateTx/099.json
-- Mozilla Public License 2.0
txEvalErrorSample = [r|
{
  "type": "jsonwsp/response",
  "version": "1.0",
  "servicename": "ogmios",
  "methodname": "EvaluateTx",
  "result": {
    "EvaluationFailure": {
      "AdditionalUtxoOverlap": [
        {
          "txId": "ae85d245a3d00bfde01f59f3c4fe0b4bfae1cb37e9cf91929eadcea4985711de",
          "index": 2
        },
        {
          "txId": "e88bd757ad5b9bedf372d8d3f0cf6c962a469db61a265f6418e1ffed86da29ec",
          "index": 0
        },
        {
          "txId": "e88bd757ad5b9bedf372d8d3f0cf6c962a469db61a265f6418e1ffed86da29ec",
          "index": 2
        }
      ]
    }
  },
  "reflection": "st"
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
