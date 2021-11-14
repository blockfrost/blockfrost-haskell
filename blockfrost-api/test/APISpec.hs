{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module APISpec
  where

import Data.Aeson (decode, eitherDecode, encode)
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_sample :: Spec
spec_sample = do
  it "parses urlVersion sample" $ do
    decode urlVersionSample
    `shouldBe`
    Just (URLVersion "https://blockfrost.io/" "0.1.0")

  it "parses health sample" $ do
    decode healthSample
    `shouldBe`
    Just (Healthy True)

  it "parses server time sample" $ do
    decode serverTimeSample
    `shouldBe`
    Just (ServerTime 1603400958.947)

  it "parses metrics sample" $ do
    eitherDecode metricsSample
    `shouldBe`
    Right metricsExpected

  it "parses metrics endpoints sample" $ do
    eitherDecode metricsEndpointsSample
    `shouldBe`
    Right metricsEndpointsExpected


urlVersionSample = [r|
{
  "url": "https://blockfrost.io/",
  "version": "0.1.0"
}
|]

healthSample = [r| { "is_healthy": true } |]

serverTimeSample = [r| { "server_time": 1603400958947 } |]

metricsSample = [r|
[
  {
    "time": 1612543884,
    "calls": 42
  },
  {
    "time": 1614523884,
    "calls": 6942
  }
]
|]

metricsExpected =
  [ Metric 1612543884 42
  , Metric 1614523884 6942
  ]

metricsEndpointsSample = [r|
[
  {
    "time": 1612543814,
    "calls": 182,
    "endpoint": "block"
  },
  {
    "time": 1612543814,
    "calls": 42,
    "endpoint": "epoch"
  },
  {
    "time": 1612543812,
    "calls": 775,
    "endpoint": "block"
  },
  {
    "time": 1612523884,
    "calls": 4,
    "endpoint": "epoch"
  },
  {
    "time": 1612553884,
    "calls": 89794,
    "endpoint": "block"
  }
]
|]

metricsEndpointsExpected :: [(Text, Metric)]
metricsEndpointsExpected =
  [ ("block", Metric 1612543814 182)
  , ("epoch", Metric 1612543814 42)
  , ("block", Metric 1612543812 775)
  , ("epoch", Metric 1612523884 4)
  , ("block", Metric 1612553884 89794)
  ]
