{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Blocks
  where

import Data.Aeson (decode, eitherDecode, encode)
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_blocks :: Spec
spec_blocks = do
  it "parses block sample" $ do
    eitherDecode blockSample
    `shouldBe`
    Right blockExpected

  it "parses afftected addresses sample" $ do
    eitherDecode affectedAddrsSample
    `shouldBe`
    Right affectedAddrsExpected

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
    "op_cert": "da905277534faf75dae41732650568af545134ee08a3c0392dbefc8096ae177c",
    "op_cert_counter": "18",
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
  , _blockOpCert = pure "da905277534faf75dae41732650568af545134ee08a3c0392dbefc8096ae177c"
  , _blockOpCertCounter = pure 18
  , _blockPreviousBlock = pure "43ebccb3ac72c7cebd0d9b755a4b08412c9f5dcb81b8a0ad1e3c197d29d47b05"
  , _blockNextBlock = pure "8367f026cf4b03e116ff8ee5daf149b55ba5a6ec6dec04803b8dc317721d15fa"
  , _blockConfirmations = 4698
  }

affectedAddrsSample = [r|
[
  {
    "address": "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv",
    "transactions": [
      {
        "tx_hash": "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0"
      }
    ]
  },
  {
    "address": "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz",
    "transactions": [
      {
        "tx_hash": "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157d0"
      }
    ]
  }
]
|]

affectedAddrsExpected :: [(Address, [TxHash])]
affectedAddrsExpected =
  [ (Address "addr1q9ld26v2lv8wvrxxmvg90pn8n8n5k6tdst06q2s856rwmvnueldzuuqmnsye359fqrk8hwvenjnqultn7djtrlft7jnq7dy7wv",
     [TxHash "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157f0"])
  , (Address "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz",
     [TxHash "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dce628516157d0"])
  ]
