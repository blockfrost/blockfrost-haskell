{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger
  where

import Data.Aeson (decode, eitherDecode, encode)
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_ledger :: Spec
spec_ledger = do
  it "parses ledger genesis sample" $ do
    eitherDecode ledgerGenesisSample
    `shouldBe`
    Right ledgerGenesisExpected

ledgerGenesisSample = [r|
{
    "active_slots_coefficient": 0.05,
    "update_quorum": 5,
    "max_lovelace_supply": "45000000000000000",
    "network_magic": 764824073,
    "epoch_length": 432000,
    "system_start": 1506203091,
    "slots_per_kes_period": 129600,
    "slot_length": 1,
    "max_kes_evolutions": 62,
    "security_param": 2160
}
|]

ledgerGenesisExpected =
  Genesis
    { _genesisActiveSlotsCoefficient = 0.05
    , _genesisUpdateQuorum = 5
    , _genesisMaxLovelaceSupply = 45_000_000_000_000_000
    , _genesisNetworkMagic = 764824073
    , _genesisEpochLength = 432_000
    , _genesisSystemStart = 1506203091
    , _genesisSlotsPerKesPeriod = 129600
    , _genesisSlotLength = 1
    , _genesisMaxKesEvolutions  = 62
    , _genesisSecurityParam  = 2160
    }


