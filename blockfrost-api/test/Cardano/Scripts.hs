{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Scripts
  where

import Data.Aeson (decode, eitherDecode, encode, object, (.=))
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_scripts :: Spec
spec_scripts = do
  it "parses script list sample" $ do
    eitherDecode scriptListSample
    `shouldBe`
    Right scriptListExpected

  it "parses script info sample" $ do
    eitherDecode scriptSample
    `shouldBe`
    Right scriptSampleExpected

  it "parses script redeemer sample" $ do
    eitherDecode scriptRedeemerSample
    `shouldBe`
    Right scriptRedeemerExpected

  it "parses script datum sample" $ do
    eitherDecode scriptDatumSample
    `shouldBe`
    Right scriptDatumExpected

  it "parses script datum cbor sample" $ do
    eitherDecode scriptDatumCBORSample
    `shouldBe`
    Right scriptDatumCBORExpected

  it "parses script JSON sample" $ do
    eitherDecode scriptJSONSample
    `shouldBe`
    Right scriptJSONExpected

  it "parses script CBOR sample" $ do
    eitherDecode scriptCBORSample
    `shouldBe`
    Right scriptCBORExpected

scriptListSample = [r|
[
  {
    "script_hash": "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
  },
  {
    "script_hash": "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
  },
  {
    "script_hash": "a6e63c0ff05c96943d1cc30bf53112ffff0f34b45986021ca058ec54"
  }
]
|]

scriptListExpected :: ScriptHashList
scriptListExpected = ScriptHashList
  [ "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
  , "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
  , "a6e63c0ff05c96943d1cc30bf53112ffff0f34b45986021ca058ec54"
  ]

scriptSample = [r|
{
  "script_hash": "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656",
  "type": "plutusV1",
  "serialised_size": 3119
}
|]

scriptSampleExpected =
  Script
    { _scriptScriptHash = "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
    , _scriptType = PlutusV1
    , _scriptSerialisedSize = Just 3119
    }

scriptRedeemerSample = [r|
{
  "tx_hash": "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0",
  "tx_index": 0,
  "purpose": "spend",
  "redeemer_data_hash": "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
  "datum_hash": "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
  "unit_mem": "1700",
  "unit_steps": "476468",
  "fee": "172033"
}
|]

scriptRedeemerExpected =
  ScriptRedeemer
    { _scriptRedeemerTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0"
    , _scriptRedeemerTxIndex = 0
    , _scriptRedeemerPurpose = Spend
    , _scriptRedeemerRedeemerDataHash = "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
    -- deprecated
    , _scriptRedeemerDatumHash = "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
    , _scriptRedeemerUnitMem = 1700
    , _scriptRedeemerUnitSteps = 476468
    , _scriptRedeemerFee = 172033
    }

scriptDatumSample = [r|
{
  "json_value": {
    "int": 42
  }
}
|]

scriptDatumExpected = ScriptDatum $ object ["int" .= (42 :: Int)]

scriptDatumCBORSample = [r|
{
  "cbor": "19a6aa"
}
|]

scriptDatumCBORExpected = ScriptDatumCBOR "19a6aa"

scriptJSONSample = [r|
{
   "json": {
     "type": "sig",
     "keyHash": "8ed9e675aaf99868736c372d5eac9f5b3deae4568f0cde6a7d9e1422"
   }
}
|]

scriptJSONExpected = ScriptJSON $ Just $
  object ["type" .= ("sig" :: String), "keyHash" .= ("8ed9e675aaf99868736c372d5eac9f5b3deae4568f0cde6a7d9e1422" :: String)]

scriptCBORSample = [r|
{
  "cbor": "4e4d01000033222220051200120011"
}
|]

scriptCBORExpected = ScriptCBOR $ Just "4e4d01000033222220051200120011"
