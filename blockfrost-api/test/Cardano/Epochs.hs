{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Epochs
  where

import Data.Aeson (decode, eitherDecode, encode)
import qualified Data.Map
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_epochs :: Spec
spec_epochs = do
  it "parses epoch info sample" $ do
    eitherDecode epochInfoSample
    `shouldBe`
    Right epochInfoExpected

  it "parses protocol params sample" $ do
    eitherDecode protocolParamsSample
    `shouldBe`
    Right protocolParamsExpected

  it "parses stake distribution sample" $ do
    eitherDecode stakeDistributionSample
    `shouldBe`
    Right stakeDistributionExpected

  it "parses pool stake distribution sample" $ do
    eitherDecode poolStakeDistributionSample
    `shouldBe`
    Right poolStakeDistributionExpected

epochInfoSample = [r|
{
    "epoch": 225,
    "start_time": 1603403091,
    "end_time": 1603835086,
    "first_block_time": 1603403092,
    "last_block_time": 1603835084,
    "block_count": 21298,
    "tx_count": 17856,
    "output": "7849943934049314",
    "fees": "4203312194",
    "active_stake": "784953934049314"
}
|]

epochInfoExpected =
  EpochInfo
    { _epochInfoEpoch = 225
    , _epochInfoStartTime = 1603403091
    , _epochInfoEndTime = 1603835086
    , _epochInfoFirstBlockTime = 1603403092
    , _epochInfoLastBlockTime = 1603835084
    , _epochInfoBlockCount = 21298
    , _epochInfoTxCount = 17856
    , _epochInfoOutput = 7849943934049314
    , _epochInfoFees = 4203312194
    , _epochInfoActiveStake = pure 784953934049314
    }

protocolParamsSample = [r|
{
    "epoch": 225,
    "min_fee_a": 44,
    "min_fee_b": 155381,
    "max_block_size": 65536,
    "max_tx_size": 16384,
    "max_block_header_size": 1100,
    "key_deposit": "2000000",
    "pool_deposit": "500000000",
    "e_max": 18,
    "n_opt": 150,
    "a0": 0.3,
    "rho": 0.003,
    "tau": 0.2,
    "decentralisation_param": 0.5,
    "extra_entropy": null,
    "protocol_major_ver": 2,
    "protocol_minor_ver": 0,
    "min_utxo": "1000000",
    "min_pool_cost": "340000000",
    "nonce": "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81",
    "cost_models": {
      "PlutusV1": {
        "addInteger-cpu-arguments-intercept": 197209,
        "addInteger-cpu-arguments-slope": 0
      },
      "PlutusV2":
      {
        "addInteger-cpu-arguments-intercept": 197209,
        "addInteger-cpu-arguments-slope": 0
      }
    },
    "price_mem": 0.0577,
    "price_step": 0.0000721,
    "max_tx_ex_mem": "10000000",
    "max_tx_ex_steps": "10000000000",
    "max_block_ex_mem": "50000000",
    "max_block_ex_steps": "40000000000",
    "max_val_size": "5000",
    "collateral_percent": 150,
    "max_collateral_inputs": 3,
    "coins_per_utxo_size": "34482",
    "coins_per_utxo_word": "34482"
}
|]

protocolParamsExpected =
  ProtocolParams
    { _protocolParamsEpoch = 225
    , _protocolParamsMinFeeA = 44
    , _protocolParamsMinFeeB = 155381
    , _protocolParamsMaxBlockSize = 65536
    , _protocolParamsMaxTxSize = 16384
    , _protocolParamsMaxBlockHeaderSize = 1100
    , _protocolParamsKeyDeposit = 2000000
    , _protocolParamsPoolDeposit = 500000000
    , _protocolParamsEMax = 18
    , _protocolParamsNOpt = 150
    , _protocolParamsA0 = 0.3
    , _protocolParamsRho = 0.003
    , _protocolParamsTau = 0.2
    , _protocolParamsDecentralisationParam = 0.5
    , _protocolParamsExtraEntropy = Nothing
    , _protocolParamsProtocolMajorVer = 2
    , _protocolParamsProtocolMinorVer = 0
    , _protocolParamsMinUtxo = 1000000
    , _protocolParamsMinPoolCost = 340000000
    , _protocolParamsNonce = "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81"
    , _protocolParamsCostModels =
        CostModels
      $ Data.Map.fromList
      [ ( PlutusV1
        , Data.Map.fromList
          [ ("addInteger-cpu-arguments-intercept", 197209)
          , ("addInteger-cpu-arguments-slope", 0)
          ]
        )
      , (PlutusV2
        , Data.Map.fromList
          [ ("addInteger-cpu-arguments-intercept", 197209)
          , ("addInteger-cpu-arguments-slope", 0)
          ]
        )
      ]
    , _protocolParamsPriceMem = 0.0577
    , _protocolParamsPriceStep = 0.0000721
    , _protocolParamsMaxTxExMem = 10000000
    , _protocolParamsMaxTxExSteps = 10000000000
    , _protocolParamsMaxBlockExMem = 50000000
    , _protocolParamsMaxBlockExSteps = 40000000000
    , _protocolParamsMaxValSize = 5000
    , _protocolParamsCollateralPercent = 150
    , _protocolParamsMaxCollateralInputs = 3
    , _protocolParamsCoinsPerUtxoSize = 34482
    -- deprecated
    , _protocolParamsCoinsPerUtxoWord = 34482
    }

stakeDistributionSample = [r|
{
  "stake_address": "stake1u9l5q5jwgelgagzyt6nuaasefgmn8pd25c8e9qpeprq0tdcp0e3uk",
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "amount": "4440295078"
}
|]

stakeDistributionExpected =
  StakeDistribution
    { _stakeDistributionStakeAddress = "stake1u9l5q5jwgelgagzyt6nuaasefgmn8pd25c8e9qpeprq0tdcp0e3uk"
    , _stakeDistributionPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    , _stakeDistributionAmount = 4440295078
    }

poolStakeDistributionSample = [r|
{
  "stake_address": "stake1u9l5q5jwgelgagzyt6nuaasefgmn8pd25c8e9qpeprq0tdcp0e3uk",
  "amount": "4440295078"
}
|]

poolStakeDistributionExpected =
  PoolStakeDistribution
    { _poolStakeDistributionStakeAddress = "stake1u9l5q5jwgelgagzyt6nuaasefgmn8pd25c8e9qpeprq0tdcp0e3uk"
    , _poolStakeDistributionAmount = 4440295078
    }


