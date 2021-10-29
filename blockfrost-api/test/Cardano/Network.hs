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
    "circulating": "32412601976210393",
    "locked": "125006953355",
    "treasury": "98635632000000",
    "reserves": "46635632000000"
  },
  "stake": {
    "live": "23204950463991654",
    "active": "22210233523456321"
  }
}
|]

networkExpected =
  Network
    NetworkSupply
       { _supplyMax = 45_000_000_000_000_000
       , _supplyTotal = 32_890_715_183_299_160
       , _supplyCirculating = 32_412_601_976_210_393
       , _supplyLocked = 125_006_953_355
       , _supplyTreasury = 98_635_632_000_000
       , _supplyReserves = 46_635_632_000_000
       }
    NetworkStake
      { _stakeLive = 23_204_950_463_991_654
      , _stakeActive = 22_210_233_523_456_321
      }
