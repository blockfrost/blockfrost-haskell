{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Pools
  where

import Data.Aeson (decode, eitherDecode, encode)
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_pools :: Spec
spec_pools = do
  it "parses pool epoch sample" $ do
    eitherDecode poolEpochSample
    `shouldBe`
    Right poolEpochExpected

  it "parses pool info sample" $ do
    eitherDecode poolInfoSample
    `shouldBe`
    Right poolInfoExpected

  it "parses pool history sample" $ do
    eitherDecode poolHistorySample
    `shouldBe`
    Right poolHistoryExpected

  it "parses pool metadata sample" $ do
    eitherDecode poolMetadataSample
    `shouldBe`
    Right poolMetadataExpected

  it "parses empty object pool metadata sample" $ do
    eitherDecode "{}"
    `shouldBe`
    Right (Nothing :: Maybe PoolMetadata)

  it "parses pool relay sample" $ do
    eitherDecode poolRelaySample
    `shouldBe`
    Right poolRelayExpected

  it "parses pool delegators sample" $ do
    eitherDecode poolDelegatorsSample
    `shouldBe`
    Right poolDelegatorsExpected

  it "parses pool updates sample" $ do
    eitherDecode poolUpdatesSample
    `shouldBe`
    Right poolUpdatesExpected

poolEpochSample = [r|
[
  {
    "pool_id": "pool19u64770wqp6s95gkajc8udheske5e6ljmpq33awxk326zjaza0q",
    "epoch": 225
  },
  {
    "pool_id": "pool1dvla4zq98hpvacv20snndupjrqhuc79zl6gjap565nku6et5zdx",
    "epoch": 215
  },
  {
    "pool_id": "pool1wvccajt4eugjtf3k0ja3exjqdj7t8egsujwhcw4tzj4rzsxzw5w",
    "epoch": 231
  }
]
|]

poolEpochExpected =
  [ PoolEpoch "pool19u64770wqp6s95gkajc8udheske5e6ljmpq33awxk326zjaza0q" 225
  , PoolEpoch "pool1dvla4zq98hpvacv20snndupjrqhuc79zl6gjap565nku6et5zdx" 215
  , PoolEpoch "pool1wvccajt4eugjtf3k0ja3exjqdj7t8egsujwhcw4tzj4rzsxzw5w" 231
  ]

poolInfoSample = [r|
{
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "hex": "0f292fcaa02b8b2f9b3c8f9fd8e0bb21abedb692a6d5058df3ef2735",
  "vrf_key": "0b5245f9934ec2151116fb8ec00f35fd00e0aa3b075c4ed12cce440f999d8233",
  "blocks_minted": 69,
  "blocks_epoch": 4,
  "live_stake": "6900000000",
  "live_size": 0.42,
  "live_saturation": 0.93,
  "live_delegators": 127,
  "active_stake": "4200000000",
  "active_size": 0.43,
  "declared_pledge": "5000000000",
  "live_pledge": "5000000001",
  "margin_cost": 0.05,
  "fixed_cost": "340000000",
  "reward_account": "stake1uxkptsa4lkr55jleztw43t37vgdn88l6ghclfwuxld2eykgpgvg3f",
  "owners": [
    "stake1u98nnlkvkk23vtvf9273uq7cph5ww6u2yq2389psuqet90sv4xv9v"
  ],
  "registration": [
    "9f83e5484f543e05b52e99988272a31da373f3aab4c064c76db96643a355d9dc",
    "7ce3b8c433bf401a190d58c8c483d8e3564dfd29ae8633c8b1b3e6c814403e95",
    "3e6e1200ce92977c3fe5996bd4d7d7e192bcb7e231bc762f9f240c76766535b9"
  ],
  "retirement": [
    "252f622976d39e646815db75a77289cf16df4ad2b287dd8e3a889ce14c13d1a8"
  ]
}
|]

poolInfoExpected =
  PoolInfo
    { _poolInfoPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    , _poolInfoHex = "0f292fcaa02b8b2f9b3c8f9fd8e0bb21abedb692a6d5058df3ef2735"
    , _poolInfoVrfKey = "0b5245f9934ec2151116fb8ec00f35fd00e0aa3b075c4ed12cce440f999d8233"
    , _poolInfoBlocksMinted = 69
    , _poolInfoBlocksEpoch = 4
    , _poolInfoLiveStake = 6900000000
    , _poolInfoLiveSize = 0.42
    , _poolInfoLiveSaturation = 0.93
    , _poolInfoLiveDelegators = 127
    , _poolInfoActiveStake = 4200000000
    , _poolInfoActiveSize = 0.43
    , _poolInfoDeclaredPledge = 5000000000
    , _poolInfoLivePledge = 5000000001
    , _poolInfoMarginCost = 0.05
    , _poolInfoFixedCost = 340000000
    , _poolInfoRewardAccount = "stake1uxkptsa4lkr55jleztw43t37vgdn88l6ghclfwuxld2eykgpgvg3f"
    , _poolInfoOwners = [ "stake1u98nnlkvkk23vtvf9273uq7cph5ww6u2yq2389psuqet90sv4xv9v" ]
    , _poolInfoRegistration =
        [ "9f83e5484f543e05b52e99988272a31da373f3aab4c064c76db96643a355d9dc"
        , "7ce3b8c433bf401a190d58c8c483d8e3564dfd29ae8633c8b1b3e6c814403e95"
        , "3e6e1200ce92977c3fe5996bd4d7d7e192bcb7e231bc762f9f240c76766535b9"
        ]
    , _poolInfoRetirement = [ "252f622976d39e646815db75a77289cf16df4ad2b287dd8e3a889ce14c13d1a8" ]
    }


poolHistorySample = [r|
{
  "epoch": 233,
  "blocks": 22,
  "active_stake": "20485965693569",
  "active_size": 1.2345,
  "delegators_count": 115,
  "rewards": "206936253674159",
  "fees": "1290968354"
}
|]

poolHistoryExpected =
  PoolHistory
    { _poolHistoryEpoch = 233
    , _poolHistoryBlocks = 22
    , _poolHistoryActiveStake = 20485965693569
    , _poolHistoryActiveSize = 1.2345
    , _poolHistoryDelegatorsCount = 115
    , _poolHistoryRewards = 206936253674159
    , _poolHistoryFees = 1290968354
    }

poolMetadataSample = [r|
{
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "hex": "0f292fcaa02b8b2f9b3c8f9fd8e0bb21abedb692a6d5058df3ef2735",
  "url": "https://stakenuts.com/mainnet.json",
  "hash": "47c0c68cb57f4a5b4a87bad896fc274678e7aea98e200fa14a1cb40c0cab1d8c",
  "ticker": "NUTS",
  "name": "Stake Nuts",
  "description": "The best pool ever",
  "homepage": "https://stakentus.com/"
}
|]

poolMetadataExpected =
  PoolMetadata
    { _poolMetadataPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
    , _poolMetadataHex = "0f292fcaa02b8b2f9b3c8f9fd8e0bb21abedb692a6d5058df3ef2735"
    , _poolMetadataUrl = Just "https://stakenuts.com/mainnet.json"
    , _poolMetadataHash = Just "47c0c68cb57f4a5b4a87bad896fc274678e7aea98e200fa14a1cb40c0cab1d8c"
    , _poolMetadataTicker = Just "NUTS"
    , _poolMetadataName = Just "Stake Nuts"
    , _poolMetadataDescription = Just "The best pool ever"
    , _poolMetadataHomepage = Just "https://stakentus.com/"
    }

poolRelaySample = [r|
{
  "ipv4": "4.4.4.4",
  "ipv6": "https://stakenuts.com/mainnet.json",
  "dns": "relay1.stakenuts.com",
  "dns_srv": "_relays._tcp.relays.stakenuts.com",
  "port": 3001
}
|]

poolRelayExpected =
  PoolRelay
    { _poolRelayIpv4 = Just "4.4.4.4"
    , _poolRelayIpv6 = Just "https://stakenuts.com/mainnet.json"
    , _poolRelayDns = Just "relay1.stakenuts.com"
    , _poolRelayDnsSrv = Just "_relays._tcp.relays.stakenuts.com"
    , _poolRelayPort = 3001
    }

poolDelegatorsSample = [r|
[
  {
    "address": "stake1ux4vspfvwuus9uwyp5p3f0ky7a30jq5j80jxse0fr7pa56sgn8kha",
    "live_stake": "1137959159981411"
  },
  {
    "address": "stake1uylayej7esmarzd4mk4aru37zh9yz0luj3g9fsvgpfaxulq564r5u",
    "live_stake": "16958865648"
  },
  {
    "address": "stake1u8lr2pnrgf8f7vrs9lt79hc3sxm8s2w4rwvgpncks3axx6q93d4ck",
    "live_stake": "18605647"
  }
]
|]

poolDelegatorsExpected =
  [ PoolDelegator
      { _poolDelegatorAddress = "stake1ux4vspfvwuus9uwyp5p3f0ky7a30jq5j80jxse0fr7pa56sgn8kha"
      , _poolDelegatorLiveStake = 1137959159981411
      }
  , PoolDelegator
      { _poolDelegatorAddress = "stake1uylayej7esmarzd4mk4aru37zh9yz0luj3g9fsvgpfaxulq564r5u"
      , _poolDelegatorLiveStake = 16958865648
      }
  , PoolDelegator
      { _poolDelegatorAddress = "stake1u8lr2pnrgf8f7vrs9lt79hc3sxm8s2w4rwvgpncks3axx6q93d4ck"
      , _poolDelegatorLiveStake = 18605647
      }
  ]

poolUpdatesSample = [r|
[
  {
    "tx_hash": "6804edf9712d2b619edb6ac86861fe93a730693183a262b165fcc1ba1bc99cad",
    "cert_index": 0,
    "action": "registered"
  },
  {
    "tx_hash": "9c190bc1ac88b2ab0c05a82d7de8b71b67a9316377e865748a89d4426c0d3005",
    "cert_index": 0,
    "action": "deregistered"
  },
  {
    "tx_hash": "e14a75b0eb2625de7055f1f580d70426311b78e0d36dd695a6bdc96c7b3d80e0",
    "cert_index": 1,
    "action": "registered"
  }
]
|]

poolUpdatesExpected =
    [ PoolUpdate
        { _poolUpdateTxHash = "6804edf9712d2b619edb6ac86861fe93a730693183a262b165fcc1ba1bc99cad"
        , _poolUpdateCertIndex = 0
        , _poolUpdateAction = PoolRegistered
        }
    , PoolUpdate
        { _poolUpdateTxHash = "9c190bc1ac88b2ab0c05a82d7de8b71b67a9316377e865748a89d4426c0d3005"
        , _poolUpdateCertIndex = 0
        , _poolUpdateAction = PoolDeregistered
        }
    , PoolUpdate
        { _poolUpdateTxHash = "e14a75b0eb2625de7055f1f580d70426311b78e0d36dd695a6bdc96c7b3d80e0"
        , _poolUpdateCertIndex = 1
        , _poolUpdateAction = PoolRegistered
        }
    ]


