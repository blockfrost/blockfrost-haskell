-- | Amount (de)serialization
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module AmountSpec
  where

import Blockfrost.Types.Shared.Amount
import Data.Aeson (decode, encode)
import Data.Default
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

spec_sample :: Spec
spec_sample = do
  it "my samples decodes to expected value" $ do
    decode mySamples `shouldBe` Just mySamplesExpected
  it "doc samples decodes to expected value" $ do
    decode docSamples `shouldBe` Just docSamplesExpected

mySamples = [r|
[
    { "unit":"lovelace"
    , "quantity":"42000000"
    }

  , { "unit":"b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
    , "quantity":"12"
    }

  , { "unit":"6804edf9712d2b619edb6ac86861fe93a730693183a262b165fcc1ba1bc99cad"
    , "quantity":"18605647"
    }
]
|]

mySamplesExpected =
  [ AdaAmount 42000000
  , AssetAmount
      $ Money.toSomeDiscrete
        (12 :: Money.Discrete'
                  "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                  '(1,1))
  , AssetAmount
      $ Money.toSomeDiscrete
        (18605647 :: Money.Discrete'
                        "6804edf9712d2b619edb6ac86861fe93a730693183a262b165fcc1ba1bc99cad"
                        '(1,1))
  ]

docSamples = [r|
[
    {
        "unit": "lovelace",
        "quantity": "42000000"
    },
    {
        "unit": "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e",
        "quantity": "12"
    }
]
|]

docSamplesExpected =
  [ AdaAmount 42000000
  , AssetAmount
      $ Money.toSomeDiscrete
        (12 :: Money.Discrete'
                  "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                  '(1,1))
  ]
