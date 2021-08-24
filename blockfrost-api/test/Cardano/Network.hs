{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Network
  where

import Data.Aeson (decode, eitherDecode, encode)
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_network :: Spec
spec_network = do
  it "parses network sample" $ do
    eitherDecode networkSample
    `shouldBe`
    Right networkExpected

networkSample = [r|
{
  "supply": {
    "max": "45000000000000000",
    "total": "32890715183299160",
    "circulating": "32412601976210393"
  },
  "stake": {
    "live": "23204950463991654",
    "active": "22210233523456321"
  }
}
|]

networkExpected =
  Network
    (NetworkSupply
      45_000_000_000_000_000
      32_890_715_183_299_160
      32_412_601_976_210_393)
    (NetworkStake
      23_204_950_463_991_654
      22_210_233_523_456_321)
