{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Accounts
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
  it "parses account sample" $ do
    eitherDecode accountSample
    `shouldBe`
    Right accountExpected

  it "parses account rewards sample" $ do
    eitherDecode accountRewardsSample
    `shouldBe`
    Right accountRewardsExpected

  it "parses account history sample" $ do
    eitherDecode accountHistorySample
    `shouldBe`
    Right accountHistoryExpected

  it "parses account delegation history sample" $ do
    eitherDecode accountDelegationHistorySample
    `shouldBe`
    Right accountDelegationHistoryExpected

  it "parses account registration sample" $ do
    eitherDecode accountRegistrationSample
    `shouldBe`
    Right accountRegistrationExpected

  it "parses account withdrawal history sample" $ do
    eitherDecode accountWithdrawalsSample
    `shouldBe`
    Right accountWithdrawalsExpected

  it "parses account mirs sample" $ do
    eitherDecode accountMirsSample
    `shouldBe`
    Right accountMirsExpected

  it "parses account associated addresses sample" $ do
    eitherDecode accountAssociatedAddressesSample
    `shouldBe`
    Right accountAssociatedAddressesExpected

accountSample = [r|
{
    "stake_address": "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7",
    "active": true,
    "active_epoch": 412,
    "controlled_amount": "619154618165",
    "rewards_sum": "319154618165",
    "withdrawals_sum": "12125369253",
    "reserves_sum": "319154618165",
    "treasury_sum": "12000000",
    "withdrawable_amount": "319154618165",
    "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
}
|]

accountExpected = AccountInfo
    { _accountInfoStakeAddress = "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7"
    , _accountInfoActive = True
    , _accountInfoActiveEpoch = pure 412
    , _accountInfoControlledAmount = 619154618165
    , _accountInfoRewardsSum = 319154618165
    , _accountInfoWithdrawalsSum = 12125369253
    , _accountInfoReservesSum = 319154618165
    , _accountInfoTreasurySum = 12000000
    , _accountInfoWithdrawableAmount = 319154618165
    , _accountInfoPoolId = pure "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    }

accountRewardsSample = [r|
[
    {
        "epoch": 215,
        "amount": "12695385",
        "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
        "type": "member"
    },
    {
        "epoch": 216,
        "amount": "12695385",
        "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
        "type": "leader"
    },
    {
        "epoch": 216,
        "amount": "3586329",
        "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
        "type": "leader"
    },
    {
        "epoch": 217,
        "amount": "0",
        "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
        "type": "leader"
    },
    {
        "epoch": 218,
        "amount": "1395265",
        "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
        "type": "pool-deposit-refund"
    }
]
|]

accountRewardsExpected =
  [ AccountReward
      { _accountRewardEpoch = 215
      , _accountRewardAmount = 12695385
      , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _accountRewardType = Member
      }
  , AccountReward
      { _accountRewardEpoch = 216
      , _accountRewardAmount = 12695385
      , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _accountRewardType = Leader
      }
  , AccountReward
      { _accountRewardEpoch = 216
      , _accountRewardAmount = 3586329
      , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _accountRewardType = Leader
      }
  , AccountReward
      { _accountRewardEpoch = 217
      , _accountRewardAmount = 0
      , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _accountRewardType = Leader
      }
  , AccountReward
      { _accountRewardEpoch = 218
      , _accountRewardAmount = 1395265
      , _accountRewardPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _accountRewardType = PoolDepositRefund
      }
  ]

accountHistorySample = [r|
[
    {
        "active_epoch": 210,
        "amount": "12695385",
        "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    },
    {
        "active_epoch": 211,
        "amount": "22695385",
        "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    }
]
|]

accountHistoryExpected =
  [ AccountHistory
      { _accountHistoryActiveEpoch = 210
      , _accountHistoryAmount = 12695385
      , _accountHistoryPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      }
  , AccountHistory
      { _accountHistoryActiveEpoch = 211
      , _accountHistoryAmount = 22695385
      , _accountHistoryPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      }
  ]

accountDelegationHistorySample = [r|
[
    {
        "active_epoch": 210,
        "tx_hash": "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531",
        "amount": "12695385",
        "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    },
    {
        "active_epoch": 242,
        "tx_hash": "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dde628516157f0",
        "amount": "12691385",
        "pool_id": "pool1kchver88u3kygsak8wgll7htr8uxn5v35lfrsyy842nkscrzyvj"
    }
]
|]

accountDelegationHistoryExpected =
  [ AccountDelegation
      { _accountDelegationActiveEpoch = 210
      , _accountDelegationTxHash = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
      , _accountDelegationAmount = 12695385
      , _accountDelegationPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      }
  , AccountDelegation
      { _accountDelegationActiveEpoch = 242
      , _accountDelegationTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dde628516157f0"
      , _accountDelegationAmount = 12691385
      , _accountDelegationPoolId = "pool1kchver88u3kygsak8wgll7htr8uxn5v35lfrsyy842nkscrzyvj"
      }
  ]

accountRegistrationSample = [r|
[
    {
        "tx_hash": "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531",
        "action": "registered"
    },
    {
        "tx_hash": "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dde628516157f0",
        "action": "deregistered"
    }
]
|]

accountRegistrationExpected =
  [ AccountRegistration
      { _accountRegistrationAction = Registered
      , _accountRegistrationTxHash = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
      }
  , AccountRegistration
      { _accountRegistrationAction = Deregistered
      , _accountRegistrationTxHash = "1a0570af966fb355a7160e4f82d5a80b8681b7955f5d44bec0dde628516157f0"
      }
  ]

accountWithdrawalsSample = [r|
[
    {
        "tx_hash": "48a9625c841eea0dd2bb6cf551eabe6523b7290c9ce34be74eedef2dd8f7ecc5",
        "amount": "454541212442"
    },
    {
        "tx_hash": "4230b0cbccf6f449f0847d8ad1d634a7a49df60d8c142bb8cc2dbc8ca03d9e34",
        "amount": "97846969"
    }
]
|]

accountWithdrawalsExpected =
  [ AccountWithdrawal
      { _accountWithdrawalAmount = 454541212442
      , _accountWithdrawalTxHash = "48a9625c841eea0dd2bb6cf551eabe6523b7290c9ce34be74eedef2dd8f7ecc5"
      }
  , AccountWithdrawal
      { _accountWithdrawalAmount = 97846969
      , _accountWithdrawalTxHash = "4230b0cbccf6f449f0847d8ad1d634a7a49df60d8c142bb8cc2dbc8ca03d9e34"
      }
  ]

accountMirsSample = [r|
[
    {
        "tx_hash": "69705bba1d687a816ff5a04ec0c358a1f1ef075ab7f9c6cc2763e792581cec6d",
        "amount": "2193707473"
    },
    {
        "tx_hash": "baaa77b63d4d7d2bb3ab02c9b85978c2092c336dede7f59e31ad65452d510c13",
        "amount": "14520198574"
    }
]
|]

accountMirsExpected =
  [ AccountMir
      { _accountMirAmount = 2193707473
      , _accountMirTxHash = "69705bba1d687a816ff5a04ec0c358a1f1ef075ab7f9c6cc2763e792581cec6d"
      }
  , AccountMir
      { _accountMirAmount = 14520198574
      , _accountMirTxHash = "baaa77b63d4d7d2bb3ab02c9b85978c2092c336dede7f59e31ad65452d510c13"
      }
  ]

accountAssociatedAddressesSample = [r|
[
  {
    "address": "addr1qx2kd28nq8ac5prwg32hhvudlwggpgfp8utlyqxu6wqgz62f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9sy0f4qd"
  },
  {
    "address": "addr1q8j55h253zcvl326sk5qdt2n8z7eghzspe0ekxgncr796s2f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9sjmd35m"
  }
]
|]

accountAssociatedAddressesExpected =
    [ AddressAssociated "addr1qx2kd28nq8ac5prwg32hhvudlwggpgfp8utlyqxu6wqgz62f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9sy0f4qd"
    , AddressAssociated "addr1q8j55h253zcvl326sk5qdt2n8z7eghzspe0ekxgncr796s2f79qsdmm5dsknt9ecr5w468r9ey0fxwkdrwh08ly3tu9sjmd35m"
    ]
