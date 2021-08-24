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
  it "parses server time sample" $ do
    decode serverTimeSample
    `shouldBe`
    Just (ServerTime 1603400958.947)

  it "parses block sample" $ do
    eitherDecode blockSample
    `shouldBe`
    Right blockExpected

  it "parses metrics sample" $ do
    eitherDecode metricsSample
    `shouldBe`
    Right metricsExpected

  it "parses metrics endpoints sample" $ do
    eitherDecode metricsEndpointsSample
    `shouldBe`
    Right metricsEndpointsExpected

serverTimeSample = [r| { "server_time": 1603400958947 } |]

blockSample = [r|
{
    "time": 1641338934,
    "height": 15243593,
    "hash": "4ea1ba291e8eef538635a53e59fddba7810d1679631cc3aed7c8e6c4091a516a",
    "slot": 412162133,
    "epoch": 425,
    "epoch_slot": 12,
    "slot_leader": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2qnikdy",
    "size": 3,
    "tx_count": 1,
    "output": "128314491794",
    "fees": "592661",
    "block_vrf": "vrf_vk1wf2k6lhujezqcfe00l6zetxpnmh9n6mwhpmhm0dvfh3fxgmdnrfqkms8ty",
    "previous_block": "43ebccb3ac72c7cebd0d9b755a4b08412c9f5dcb81b8a0ad1e3c197d29d47b05",
    "next_block": "8367f026cf4b03e116ff8ee5daf149b55ba5a6ec6dec04803b8dc317721d15fa",
    "confirmations": 4698
}
|]

blockExpected = Block
  { _blockTime = 1641338934
  , _blockHeight = pure 15243593
  , _blockHash = "4ea1ba291e8eef538635a53e59fddba7810d1679631cc3aed7c8e6c4091a516a"
  , _blockSlot = pure 412162133
  , _blockEpoch = pure 425
  , _blockEpochSlot = pure 12
  , _blockSlotLeader = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2qnikdy"
  , _blockSize = 3
  , _blockTxCount = 1
  , _blockOutput = pure $ 128314491794
  , _blockFees = pure $ 592661
  , _blockBlockVrf = pure "vrf_vk1wf2k6lhujezqcfe00l6zetxpnmh9n6mwhpmhm0dvfh3fxgmdnrfqkms8ty"
  , _blockPreviousBlock = pure "43ebccb3ac72c7cebd0d9b755a4b08412c9f5dcb81b8a0ad1e3c197d29d47b05"
  , _blockNextBlock = pure "8367f026cf4b03e116ff8ee5daf149b55ba5a6ec6dec04803b8dc317721d15fa"
  , _blockConfirmations = 4698
  }

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
