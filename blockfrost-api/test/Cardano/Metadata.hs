{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Metadata
  where

import Data.Aeson (decode, eitherDecode, encode, object, (.=))
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_metadata :: Spec
spec_metadata = do
  it "parses tx meta sample" $ do
    eitherDecode txMetaSample
    `shouldBe`
    Right txMetaExpected

  it "parses tx meta JSON sample" $ do
    eitherDecode txMetaJSONSample
    `shouldBe`
    Right txMetaJSONExpected

  it "parses tx meta CBOR sample" $ do
    eitherDecode txMetaCBORSample
    `shouldBe`
    Right txMetaCBORExpected

txMetaSample = [r|
[
  {
    "label": "1990",
    "cip10": null,
    "count": "1"
  },
  {
    "label": "1967",
    "cip10": "nut.link metadata oracles registry",
    "count": "3"
  },
  {
    "label": "1968",
    "cip10": "nut.link metadata oracles data points",
    "count": "16321"
  }
]
|]

txMetaExpected =
  [ TxMeta "1990" Nothing 1
  , TxMeta "1967" (Just "nut.link metadata oracles registry") 3
  , TxMeta "1968" (Just "nut.link metadata oracles data points") 16321
  ]

txMetaJSONSample = [r|
[
  {
    "tx_hash": "257d75c8ddb0434e9b63e29ebb6241add2b835a307aa33aedba2effe09ed4ec8",
    "json_metadata": {
      "ADAUSD": [
        {
          "value": "0.10409800535729975",
          "source": "ergoOracles"
        }
      ]
    }
  },
  {
    "tx_hash": "e865f2cc01ca7381cf98dcdc4de07a5e8674b8ea16e6a18e3ed60c186fde2b9c",
    "json_metadata": {
      "ADAUSD": [
        {
          "value": "0.15409850555139935",
          "source": "ergoOracles"
        }
      ]
    }
  },
  {
    "tx_hash": "4237501da3cfdd53ade91e8911e764bd0699d88fd43b12f44a1f459b89bc91be",
    "json_metadata": null
  }
]
|]

txMetaJSONExpected =
  let oracleMeta val =
        object [
          "ADAUSD" .=
            [ object [
                        "value" .= (val :: Text)
                      , "source" .= ("ergoOracles" :: Text)
                      ]
            ]
        ]
  in
  [ TxMetaJSON
      "257d75c8ddb0434e9b63e29ebb6241add2b835a307aa33aedba2effe09ed4ec8"
      (Just $ oracleMeta "0.10409800535729975")
  , TxMetaJSON
      "e865f2cc01ca7381cf98dcdc4de07a5e8674b8ea16e6a18e3ed60c186fde2b9c"
      (Just $ oracleMeta "0.15409850555139935")
  , TxMetaJSON
      "4237501da3cfdd53ade91e8911e764bd0699d88fd43b12f44a1f459b89bc91be"
      Nothing
  ]

txMetaCBORSample = [r|
[
  {
    "tx_hash": "257d75c8ddb0434e9b63e29ebb6241add2b835a307aa33aedba2effe09ed4ec8",
    "cbor_metadata": null
  },
  {
    "tx_hash": "e865f2cc01ca7381cf98dcdc4de07a5e8674b8ea16e6a18e3ed60c186fde2b9c",
    "cbor_metadata": null
  },
  {
    "tx_hash": "4237501da3cfdd53ade91e8911e764bd0699d88fd43b12f44a1f459b89bc91be",
    "cbor_metadata": "\\xa100a16b436f6d62696e6174696f6e8601010101010c",
    "metadata": "a100a16b436f6d62696e6174696f6e8601010101010c"
  }
]
|]

txMetaCBORExpected =
  [ TxMetaCBOR
      "257d75c8ddb0434e9b63e29ebb6241add2b835a307aa33aedba2effe09ed4ec8"
      Nothing
  , TxMetaCBOR
      "e865f2cc01ca7381cf98dcdc4de07a5e8674b8ea16e6a18e3ed60c186fde2b9c"
      Nothing
  , TxMetaCBOR
      "4237501da3cfdd53ade91e8911e764bd0699d88fd43b12f44a1f459b89bc91be"
      (Just "a100a16b436f6d62696e6174696f6e8601010101010c")
  ]
