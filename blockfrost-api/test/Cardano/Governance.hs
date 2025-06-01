{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Governance
  where

import Data.Aeson (decode, eitherDecode, encode, object, (.=))
import qualified Data.Map
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_epochs :: Spec
spec_epochs = do
  it "parses DReps sample" $ do
    eitherDecode dRepsSample
    `shouldBe`
    Right dRepsExpected

  it "parses DRepInfo sample" $ do
    eitherDecode dRepInfoSample
    `shouldBe`
    Right dRepInfoExpected

  it "parses DRepDelegators sample" $ do
    eitherDecode dRepDelegatorsSample
    `shouldBe`
    Right dRepDelegatorsExpected

  it "parses DRepMeta sample" $ do
    eitherDecode dRepMetaSample
    `shouldBe`
    Right dRepMetaExpected

  it "parses DRepUpdates sample" $ do
      eitherDecode dRepUpdatesSample
      `shouldBe`
      Right dRepUpdatesExpected

  it "parses DRepVotes sample" $ do
      eitherDecode dRepVotesSample
      `shouldBe`
      Right dRepVotesExpected

  it "parses Proposals sample" $ do
      eitherDecode dRepProposalsSample
      `shouldBe`
      Right dRepProposalsExpected

  it "parses ProposalInfo sample" $ do
      eitherDecode proposalInfoSample
      `shouldBe`
      Right proposalInfoExpected

  it "parses ParamProposal sample" $ do
    eitherDecode paramProposalSample
    `shouldBe`
    Right paramProposalExpected

  it "parses WithdrawalProposals sample" $ do
    eitherDecode withdrawalProposalSample
    `shouldBe`
    Right withdrawalProposalExpected

  it "parses ProposalVotes sample" $ do
    eitherDecode proposalVotesSample
    `shouldBe`
    Right proposalVotesExpected

  it "parses ProposalMeta sample" $ do
    eitherDecode proposalMetaSample
    `shouldBe`
    Right proposalMetaExpected

dRepsSample = [r|
[
  {
    "drep_id": "drep1mvdu8slennngja7w4un6knwezufra70887zuxpprd64jxfveahn",
    "hex": "db1bc3c3f99ce68977ceaf27ab4dd917123ef9e73f85c304236eab23"
  },
  {
    "drep_id": "drep1cxayn4fgy27yaucvhamsvqj3v6835mh3tjjx6x8hdnr4",
    "hex": "c1ba49d52822bc4ef30cbf77060251668f1a6ef15ca46d18f76cc758"
  }
]
|]

dRepsExpected :: [DRep]
dRepsExpected =
  [ DRep
      { _dRepDrepId = "drep1mvdu8slennngja7w4un6knwezufra70887zuxpprd64jxfveahn"
      , _dRepHex    = "db1bc3c3f99ce68977ceaf27ab4dd917123ef9e73f85c304236eab23"
      }
  , DRep
      { _dRepDrepId = "drep1cxayn4fgy27yaucvhamsvqj3v6835mh3tjjx6x8hdnr4"
      , _dRepHex    = "c1ba49d52822bc4ef30cbf77060251668f1a6ef15ca46d18f76cc758"
      }
  ]

dRepInfoSample = [r|
{
  "drep_id": "drep15cfxz9exyn5rx0807zvxfrvslrjqfchrd4d47kv9e0f46uedqtc",
  "hex": "a61261172624e8333ceff098648d90f8e404e2e36d5b5f5985cbd35d",
  "amount": "2000000",
  "active": true,
  "active_epoch": 420,
  "has_script": true,
  "last_active_epoch": 509,
  "retired": false,
  "expired": false
}
|]

dRepInfoExpected :: DRepInfo
dRepInfoExpected =
  DRepInfo
    { _dRepInfoDrepId          = "drep15cfxz9exyn5rx0807zvxfrvslrjqfchrd4d47kv9e0f46uedqtc"
    , _dRepInfoHex             = "a61261172624e8333ceff098648d90f8e404e2e36d5b5f5985cbd35d"
    , _dRepInfoAmount          = 2000000
    , _dRepInfoHasScript       = True
    , _dRepInfoRetired         = False
    , _dRepInfoExpired         = False
    , _dRepInfoLastActiveEpoch = Just 509
    }


dRepDelegatorsSample = [r|
[
  {
    "address": "stake1ux4vspfvwuus9uwyp5p3f0ky7a30jq5j80jxse0fr7pa56sgn8kha",
    "amount": "1137959159981411"
  },
  {
    "address": "stake1uylayej7esmarzd4mk4aru37zh9yz0luj3g9fsvgpfaxulq564r5u",
    "amount": "16958865648"
  },
  {
    "address": "stake1u8lr2pnrgf8f7vrs9lt79hc3sxm8s2w4rwvgpncks3axx6q93d4ck",
    "amount": "18605647"
  }
]
|]

dRepDelegatorsExpected :: [DRepDelegator]
dRepDelegatorsExpected =
    [ DRepDelegator
        { _dRepDelegatorAddress = "stake1ux4vspfvwuus9uwyp5p3f0ky7a30jq5j80jxse0fr7pa56sgn8kha"
        , _dRepDelegatorAmount  = 1137959159981411
        }
    , DRepDelegator
        { _dRepDelegatorAddress = "stake1uylayej7esmarzd4mk4aru37zh9yz0luj3g9fsvgpfaxulq564r5u"
        , _dRepDelegatorAmount  = 16958865648
        }
    , DRepDelegator
        { _dRepDelegatorAddress = "stake1u8lr2pnrgf8f7vrs9lt79hc3sxm8s2w4rwvgpncks3axx6q93d4ck"
        , _dRepDelegatorAmount  = 18605647
        }
    ]

dRepMetaSample = [r|
{
  "drep_id": "drep15cfxz9exyn5rx0807zvxfrvslrjqfchrd4d47kv9e0f46uedqtc",
  "hex": "a61261172624e8333ceff098648d90f8e404e2e36d5b5f5985cbd35d",
  "url": "https://aaa.xyz/drep.json",
  "hash": "a14a5ad4f36bddc00f92ddb39fd9ac633c0fd43f8bfa57758f9163d10ef916de",
  "json_metadata": {
    "@context": {
      "CIP100": "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#",
      "CIP119": "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#",
      "hashAlgorithm": "CIP100:hashAlgorithm",
      "body": {
        "@id": "CIP119:body",
        "@context": {
          "references": {
            "@id": "CIP119:references",
            "@container": "@set",
            "@context": {
              "GovernanceMetadata": "CIP100:GovernanceMetadataReference",
              "Other": "CIP100:OtherReference",
              "label": "CIP100:reference-label",
              "uri": "CIP100:reference-uri"
            }
          },
          "paymentAddress": "CIP119:paymentAddress",
          "givenName": "CIP119:givenName",
          "image": {
            "@id": "CIP119:image",
            "@context": {
              "ImageObject": "https://schema.org/ImageObject"
            }
          },
          "objectives": "CIP119:objectives",
          "motivations": "CIP119:motivations",
          "qualifications": "CIP119:qualifications"
        }
      }
    },
    "hashAlgorithm": "blake2b-256",
    "body": {
      "paymentAddress": "addr1q86dnpkva4mm859c8ur7tjxn57zgsu6vg8pdetkdve3fsacnq7twy06u2ev5759vutpjgzfryx0ud8hzedhzerava35qwh3x34",
      "givenName": "Ryan Williams",
      "image": {
        "@type": "ImageObject",
        "contentUrl": "https://avatars.githubusercontent.com/u/44342099?v=4",
        "sha256": "2a21e4f7b20c8c72f573707b068fb8fc6d8c64d5035c4e18ecae287947fe2b2e"
      },
      "objectives": "Buy myself an island.",
      "motivations": "I really would like to own an island.",
      "qualifications": "I have my 100m swimming badge, so I would be qualified to be able to swim around island.",
      "references": [
        {
          "@type": "Other",
          "label": "A cool island for Ryan",
          "uri": "https://www.google.com/maps/place/World's+only+5th+order+recursive+island/@62.6511465,-97.7946829,15.75z/data=!4m14!1m7!3m6!1s0x5216a167810cee39:0x11431abdfe4c7421!2sWorld's+only+5th+order+recursive+island!8m2!3d62.651114!4d-97.7872244!16s%2Fg%2F11spwk2b6n!3m5!1s0x5216a167810cee39:0x11431abdfe4c7421!8m2!3d62.651114!4d-97.7872244!16s%2Fg%2F11spwk2b6n?authuser=0&entry=ttu"
        },
        {
          "@type": "Link",
          "label": "Ryan's Twitter",
          "uri": "https://twitter.com/Ryun1_"
        }
      ]
    }
  },
  "bytes": "\\x7b0a20202240636f6e74657874223a207b0a2020202022406c616e6775616765223a2022656e2d7573222c0a2020202022434950313030223a202268747470733a2f2f6769746875622e636f6d2f63617264616e6f2d666f756e646174696f6e2f434950732f626c6f622f6d61737465722f4349502d303130302f524541444d452e6"
}
|]

dRepMetaExpected :: DRepMeta
dRepMetaExpected =
  DRepMeta
    { _dRepMetaDrepId          = "drep15cfxz9exyn5rx0807zvxfrvslrjqfchrd4d47kv9e0f46uedqtc"
    , _dRepMetaHex             = "a61261172624e8333ceff098648d90f8e404e2e36d5b5f5985cbd35d"
    , _dRepMetaUrl             = "https://aaa.xyz/drep.json"
    , _dRepMetaHash            = "a14a5ad4f36bddc00f92ddb39fd9ac633c0fd43f8bfa57758f9163d10ef916de"
    , _dRepMetaJsonMetadata    = object
      [ "@context" .= object
          [ "CIP100" .= ("https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#" :: Text)
          , "CIP119" .= ("https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#" :: Text)
          , "hashAlgorithm" .= ("CIP100:hashAlgorithm" :: Text)
          , "body" .= object
            [ "@id" .= ("CIP119:body" :: Text)
            , "@context" .= object
                [ "references" .= object
                    [ "@id" .= ("CIP119:references" :: Text)
                    , "@container" .= ("@set" :: Text)
                    , "@context" .= object
                        [ "GovernanceMetadata" .= ("CIP100:GovernanceMetadataReference" :: Text)
                        , "Other" .= ("CIP100:OtherReference" :: Text)
                        , "label" .= ("CIP100:reference-label" :: Text)
                        , "uri" .= ("CIP100:reference-uri" :: Text)
                        ]
                    ]
                    , "paymentAddress" .= ("CIP119:paymentAddress" :: Text)
                    , "givenName" .= ("CIP119:givenName" :: Text)
                    , "image" .= object
                        [ "@id" .= ("CIP119:image" :: Text)
                        , "@context" .= object
                            [ "ImageObject" .= ("https://schema.org/ImageObject" :: Text) ]
                        ]
                    , "objectives" .= ("CIP119:objectives" :: Text)
                    , "motivations" .= ("CIP119:motivations" :: Text)
                    , "qualifications" .= ("CIP119:qualifications" :: Text)
                    ]
                ]
            ]
        , "hashAlgorithm" .= ("blake2b-256" :: Text)
        , "body" .= object
            [ "paymentAddress" .= ("addr1q86dnpkva4mm859c8ur7tjxn57zgsu6vg8pdetkdve3fsacnq7twy06u2ev5759vutpjgzfryx0ud8hzedhzerava35qwh3x34" :: Text)
            , "givenName" .= ("Ryan Williams" :: Text)
            , "image" .= object
                [ "@type" .= ("ImageObject" :: Text)
                , "contentUrl" .= ("https://avatars.githubusercontent.com/u/44342099?v=4" :: Text)
                , "sha256" .= ("2a21e4f7b20c8c72f573707b068fb8fc6d8c64d5035c4e18ecae287947fe2b2e" :: Text)
                ]
            , "objectives" .= ("Buy myself an island." :: Text)
            , "motivations" .= ("I really would like to own an island." :: Text)
            , "qualifications" .= ("I have my 100m swimming badge, so I would be qualified to be able to swim around island." :: Text)
            , "references" .=
                [ object
                    [ "@type" .= ("Other" :: Text)
                    , "label" .= ("A cool island for Ryan" :: Text)
                    , "uri" .= ("https://www.google.com/maps/place/World's+only+5th+order+recursive+island/@62.6511465,-97.7946829,15.75z/data=!4m14!1m7!3m6!1s0x5216a167810cee39:0x11431abdfe4c7421!2sWorld's+only+5th+order+recursive+island!8m2!3d62.651114!4d-97.7872244!16s%2Fg%2F11spwk2b6n!3m5!1s0x5216a167810cee39:0x11431abdfe4c7421!8m2!3d62.651114!4d-97.7872244!16s%2Fg%2F11spwk2b6n?authuser=0&entry=ttu" :: Text)
                    ]
                , object
                    [ "@type" .= ("Link" :: Text)
                    , "label" .= ("Ryan's Twitter" :: Text)
                    , "uri" .= ("https://twitter.com/Ryun1_" :: Text)
                    ]
                ]
            ]
        ]
    , _dRepMetaBytes           = "\\x7b0a20202240636f6e74657874223a207b0a2020202022406c616e6775616765223a2022656e2d7573222c0a2020202022434950313030223a202268747470733a2f2f6769746875622e636f6d2f63617264616e6f2d666f756e646174696f6e2f434950732f626c6f622f6d61737465722f4349502d303130302f524541444d452e6"
    }

dRepUpdatesSample = [r|
[
  {
    "tx_hash": "f4097fbdb87ab7c7ab44b30d4e2b81713a058488975d1ab8b05c381dd946a393",
    "cert_index": 0,
    "action": "registered"
  },
  {
    "tx_hash": "dd3243af975be4b5bedce4e5f5b483b2386d5ad207d05e0289c1df0eb261447e",
    "cert_index": 0,
    "action": "deregistered"
  }
]
|]

dRepUpdatesExpected :: [DRepUpdate]
dRepUpdatesExpected =
  [ DRepUpdate
      { _dRepUpdateTxHash = "f4097fbdb87ab7c7ab44b30d4e2b81713a058488975d1ab8b05c381dd946a393"
      , _dRepUpdateCertIndex = 0
      , _dRepUpdateAction = DRepRegistrationAction_Registered
      }
  , DRepUpdate
      { _dRepUpdateTxHash = "dd3243af975be4b5bedce4e5f5b483b2386d5ad207d05e0289c1df0eb261447e"
      , _dRepUpdateCertIndex = 0
      , _dRepUpdateAction = DRepRegistrationAction_Deregistered
      }
  ]

dRepVotesSample = [r|
[
  {
    "tx_hash": "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5",
    "cert_index": 2,
    "vote": "yes"
  },
  {
    "tx_hash": "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5",
    "cert_index": 3,
    "vote": "abstain"
  }
]
|]

dRepVotesExpected :: [DRepVote]
dRepVotesExpected =
  [ DRepVote
      { _dRepVoteTxHash = "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5"
      , _dRepVoteCertIndex = 2
      , _dRepVoteAction = VotingAction_Yes
      }
  , DRepVote
      { _dRepVoteTxHash = "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5"
      , _dRepVoteCertIndex = 3
      , _dRepVoteAction = VotingAction_Abstain
      }
  ]


dRepProposalsSample = [r|
[
  {
    "tx_hash": "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531",
    "cert_index": 1,
    "governance_type": "treasury_withdrawals"
  },
  {
    "tx_hash": "71317e951b20aa46e9fbf45a46a6e950d5723a481225519655bf6c60",
    "cert_index": 4,
    "governance_type": "no_confidence"
  }
]
|]

dRepProposalsExpected :: [Proposal]
dRepProposalsExpected =
  [ Proposal
      { _dRepProposalTxHash = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
      , _dRepProposalCertIndex = 1
      , _dRepProposalAction = ProposalAction_TreasuryWithdrawals
      }
  , Proposal
      { _dRepProposalTxHash = "71317e951b20aa46e9fbf45a46a6e950d5723a481225519655bf6c60"
      , _dRepProposalCertIndex = 4
      , _dRepProposalAction = ProposalAction_NoConfidence
      }
  ]

proposalInfoSample = [r|
{
  "tx_hash": "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531",
  "cert_index": 1,
  "governance_type": "treasury_withdrawals",
  "deposit": "12000",
  "return_address": "stake_test1urd3hs7rlxwwdzthe6hj026dmyt3y0heuulctscyydh2kgck6nkmz",
  "governance_description":
    {"tag" :"InfoAction" },
  "ratified_epoch": null,
  "enacted_epoch": 123,
  "dropped_epoch": null,
  "expired_epoch": null,
  "expiration": 120
}
|]

proposalInfoExpected :: ProposalInfo
proposalInfoExpected =
  ProposalInfo
    { _proposalInfoTxHash                = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
    , _proposalInfoCertIndex             = 1
    , _proposalInfoGovernanceType        = ProposalAction_TreasuryWithdrawals
    , _proposalInfoDeposit               = 12000
    , _proposalInfoReturnAddress         = "stake_test1urd3hs7rlxwwdzthe6hj026dmyt3y0heuulctscyydh2kgck6nkmz"
    , _proposalInfoGovernanceDescription = Just $ object [ "tag" .= ("InfoAction" :: Text) ]
    , _proposalInfoRatifiedEpoch         = Nothing
    , _proposalInfoEnactedEpoch          = Just 123
    , _proposalInfoDroppedEpoch          = Nothing
    , _proposalInfoExpiredEpoch          = Nothing
    , _proposalInfoExpiration            = 120
    }

paramProposalSample = [r|
{
  "tx_hash": "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531",
  "cert_index": 2,
  "parameters": {
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
    "cost_models_raw": {
      "PlutusV1": [
        197209,
        0
      ],
      "PlutusV2": [
        197209,
        0
      ],
      "PlutusV3": [
        197209,
        0
      ]
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
    "coins_per_utxo_word": "34482",
    "pvt_motion_no_confidence": 0.51,
    "pvt_committee_normal": 0.51,
    "pvt_committee_no_confidence": 0.51,
    "pvt_hard_fork_initiation": 0.51,
    "dvt_motion_no_confidence": 0.67,
    "dvt_committee_normal": 0.67,
    "dvt_committee_no_confidence": 0.6,
    "dvt_update_to_constitution": 0.75,
    "dvt_hard_fork_initiation": 0.6,
    "dvt_p_p_network_group": 0.67,
    "dvt_p_p_economic_group": 0.67,
    "dvt_p_p_technical_group": 0.67,
    "dvt_p_p_gov_group": 0.75,
    "dvt_treasury_withdrawal": 0.67,
    "committee_min_size": "7",
    "committee_max_term_length": "146",
    "gov_action_lifetime": "6",
    "gov_action_deposit": "100000000000",
    "drep_deposit": "500000000",
    "drep_activity": "20",
    "pvtpp_security_group": 0.51,
    "min_fee_ref_script_cost_per_byte": 15
  }
}
|]

paramProposalExpected :: ParamProposal
paramProposalExpected =
  ParamProposal
    { _paramProposalTxHash     = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
    , _paramProposalCertIndex  = 2
    , _paramProposalParameters =
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
        , _protocolParamsCostModelsRaw =
            CostModelsRaw
          $ Data.Map.fromList
          [ ( PlutusV1
            , [ 197209
              , 0
              ]
            )
          , (PlutusV2
            , [ 197209
              , 0
              ]
            )
          , (PlutusV3
            , [ 197209
              , 0
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
        , _protocolParamsPvtMotionNoConfidence = Just 0.51
        , _protocolParamsPvtCommitteeNormal = Just 0.51
        , _protocolParamsPvtCommitteeNoConfidence = Just 0.51
        , _protocolParamsPvtHardForkInitiation = Just 0.51
        , _protocolParamsPvtppSecurityGroup = Just 0.51
        , _protocolParamsDvtMotionNoConfidence = Just 0.67
        , _protocolParamsDvtCommitteeNormal = Just 0.67
        , _protocolParamsDvtCommitteeNoConfidence = Just 0.6
        , _protocolParamsDvtUpdateToConstitution = Just 0.75
        , _protocolParamsDvtHardForkInitiation = Just 0.6
        , _protocolParamsDvtPPNetworkGroup = Just 0.67
        , _protocolParamsDvtPPEconomicGroup = Just 0.67
        , _protocolParamsDvtPPTechnicalGroup = Just 0.67
        , _protocolParamsDvtPPGovGroup = Just 0.75
        , _protocolParamsDvtTreasuryWithdrawal = Just 0.67
        , _protocolParamsCommitteeMinSize = Just 7
        , _protocolParamsCommitteeMaxTermLength = Just 146
        , _protocolParamsGovActionLifetime = Just 6
        , _protocolParamsGovActionDeposit = Just 100000000000
        , _protocolParamsDrepDeposit = Just 500000000
        , _protocolParamsDrepActivity = Just 20
        , _protocolParamsMinFeeRefScriptCostPerByte = Just 15
        }
    }

withdrawalProposalSample = [r|
[
  {
    "stake_address": "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7",
    "amount": "454541212442"
  },
  {
    "stake_address": "stake1xx2g2c9dx2nhhehyrezyxpkstoppcqmu9hk63qgfkccw5rqttygt2",
    "amount": "97846969"
  }
]
|]

withdrawalProposalExpected :: [WithdrawalProposal]
withdrawalProposalExpected =
  [ WithdrawalProposal
      { _withdrawalProposalStakeAddress = "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7"
      , _withdrawalProposalAmount       = 454541212442
      }
  , WithdrawalProposal
      { _withdrawalProposalStakeAddress = "stake1xx2g2c9dx2nhhehyrezyxpkstoppcqmu9hk63qgfkccw5rqttygt2"
      , _withdrawalProposalAmount       = 97846969
      }
  ]

proposalVotesSample = [r|
[
  {
    "tx_hash": "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5",
    "cert_index": 2,
    "voter_role": "drep",
    "voter": "drep1mvdu8slennngja7w4un6knwezufra70887zuxpprd64jxfveahn",
    "vote": "yes"
  },
  {
    "tx_hash": "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5",
    "cert_index": 3,
    "voter_role": "constitutional_committee",
    "voter": "53a42debdc7ffd90085ab7fd9800b63e6d1c9ac481ba6eb7b6a844e4",
    "vote": "abstain"
  }
]
|]

proposalVotesExpected :: [ProposalVote]
proposalVotesExpected =
    [ ProposalVote
        { _proposalVoteTxHash    = "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5"
        , _proposalVoteCertIndex = 2
        , _proposalVoteVoterRole = VoterRole_DRep
        , _proposalVoteVoter     = DRepId_Bech32 "drep1mvdu8slennngja7w4un6knwezufra70887zuxpprd64jxfveahn"
        , _proposalVoteAction    = VotingAction_Yes
        }
    , ProposalVote
        { _proposalVoteTxHash    = "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5"
        , _proposalVoteCertIndex = 3
        , _proposalVoteVoterRole = VoterRole_ConstitutionalCommittee
        , _proposalVoteVoter     = DRepId_Hex "53a42debdc7ffd90085ab7fd9800b63e6d1c9ac481ba6eb7b6a844e4"
        , _proposalVoteAction    = VotingAction_Abstain
        }
    ]

proposalMetaSample = [r|
{
  "tx_hash": "257d75c8ddb0434e9b63e29ebb6241add2b835a307aa33aedba2effe09ed4ec8",
  "cert_index": 2,
  "url": "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json",
  "hash": "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81",
  "json_metadata": {
    "body": {
      "title": "Hardfork to Protocol version 10",
      "abstract": "Let's have sanchoNet in full governance as soon as possible",
      "rationale": "Let's keep testing stuff",
      "motivation": "PV9 is not as fun as PV10",
      "references": [
        {
          "uri": "",
          "@type": "Other",
          "label": "Hardfork to PV10"
        }
      ]
    },
    "authors": [
      {
        "name": "Carlos",
        "witness": {
          "publicKey": "7ea09a34aebb13c9841c71397b1cabfec5ddf950405293dee496cac2f437480a",
          "signature": "a476985b4cc0d457f247797611799a6f6a80fc8cb7ec9dcb5a8223888d0618e30de165f3d869c4a0d9107d8a5b612ad7c5e42441907f5b91796f0d7187d64a01",
          "witnessAlgorithm": "ed25519"
        }
      }
    ],
    "@context": {
      "body": {
        "@id": "CIP108:body",
        "@context": {
          "title": "CIP108:title",
          "abstract": "CIP108:abstract",
          "rationale": "CIP108:rationale",
          "motivation": "CIP108:motivation",
          "references": {
            "@id": "CIP108:references",
            "@context": {
              "uri": "CIP100:reference-uri",
              "Other": "CIP100:OtherReference",
              "label": "CIP100:reference-label",
              "referenceHash": {
                "@id": "CIP108:referenceHash",
                "@context": {
                  "hashDigest": "CIP108:hashDigest",
                  "hashAlgorithm": "CIP100:hashAlgorithm"
                }
              },
              "GovernanceMetadata": "CIP100:GovernanceMetadataReference"
            },
            "@container": "@set"
          }
        }
      },
      "CIP100": "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#",
      "CIP108": "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#",
      "authors": {
        "@id": "CIP100:authors",
        "@context": {
          "name": "http://xmlns.com/foaf/0.1/name",
          "witness": {
            "@id": "CIP100:witness",
            "@context": {
              "publicKey": "CIP100:publicKey",
              "signature": "CIP100:signature",
              "witnessAlgorithm": "CIP100:witnessAlgorithm"
            }
          }
        },
        "@container": "@set"
      },
      "@language": "en-us",
      "hashAlgorithm": "CIP100:hashAlgorithm"
    },
    "hashAlgorithm": "blake2b-256"
  },
  "bytes": "\\x7b0a20202240636f6e74657874223a207b0a2020202022406c616e6775616765223a2022656e2d7573222c0a2020202022434950313030223a202268747470733a2f2f6769746875622e636f6d2f63617264616e6f2d666f756e646174696f6e2f434950732f626c6f622f6d61737465722f4349502d303130302f524541444d452e6d6423222c0a2020202022434950313038223a202268747470733a2f2f6769746875622e636f6d2f63617264616e6f2d666f756e646174696f6e2f434950732f626c6f622f6d61737465722f4349502d303130382f524541444d452e6d6423222c0a202020202268617368416c676f726974686d223a20224349503130303a68617368416c676f726974686d222c0a2020202022626f6479223a207b0a20202020202022406964223a20224349503130383a626f6479222c0a2020202020202240636f6e74657874223a207b0a2020202020202020227265666572656e636573223a207b0a2020202020202020202022406964223a20224349503130383a7265666572656e636573222c0a202020202020202020202240636f6e7461696e6572223a202240736574222c0a202020202020202020202240636f6e74657874223a207b0a20202020202020202020202022476f7665726e616e63654d65746164617461223a20224349503130303a476f7665726e616e63654d657461646174615265666572656e6365222c0a202020202020202020202020224f74686572223a20224349503130303a4f746865725265666572656e6365222c0a202020202020202020202020226c6162656c223a20224349503130303a7265666572656e63652d6c6162656c222c0a20202020202020202020202022757269223a20224349503130303a7265666572656e63652d757269222c0a202020202020202020202020227265666572656e636548617368223a207b0a202020202020202020202020202022406964223a20224349503130383a7265666572656e636548617368222c0a20202020202020202020202020202240636f6e74657874223a207b0a202020202020202020202020202020202268617368446967657374223a20224349503130383a68617368446967657374222c0a202020202020202020202020202020202268617368416c676f726974686d223a20224349503130303a68617368416c676f726974686d220a20202020202020202020202020207d0a2020202020202020202020207d0a202020202020202020207d0a20202020202020207d2c0a2020202020202020227469746c65223a20224349503130383a7469746c65222c0a2020202020202020226162737472616374223a20224349503130383a6162737472616374222c0a2020202020202020226d6f7469766174696f6e223a20224349503130383a6d6f7469766174696f6e222c0a202020202020202022726174696f6e616c65223a20224349503130383a726174696f6e616c65220a2020202020207d0a202020207d2c0a2020202022617574686f7273223a207b0a20202020202022406964223a20224349503130303a617574686f7273222c0a2020202020202240636f6e7461696e6572223a202240736574222c0a2020202020202240636f6e74657874223a207b0a2020202020202020226e616d65223a2022687474703a2f2f786d6c6e732e636f6d2f666f61662f302e312f6e616d65222c0a2020202020202020227769746e657373223a207b0a2020202020202020202022406964223a20224349503130303a7769746e657373222c0a202020202020202020202240636f6e74657874223a207b0a202020202020202020202020227769746e657373416c676f726974686d223a20224349503130303a7769746e657373416c676f726974686d222c0a202020202020202020202020227075626c69634b6579223a20224349503130303a7075626c69634b6579222c0a202020202020202020202020227369676e6174757265223a20224349503130303a7369676e6174757265220a202020202020202020207d0a20202020202020207d0a2020202020207d0a202020207d0a20207d2c0a20202268617368416c676f726974686d223a2022626c616b6532622d323536222c0a202022626f6479223a207b0a20202020227469746c65223a202248617264666f726b20746f2050726f746f636f6c2076657273696f6e203130222c0a20202020226162737472616374223a20224c6574277320686176652073616e63686f4e657420696e2066756c6c20676f7665726e616e636520617320736f6f6e20617320706f737369626c65222c0a20202020226d6f7469766174696f6e223a2022505639206973206e6f742061732066756e2061732050563130222c0a2020202022726174696f6e616c65223a20224c65742773206b6565702074657374696e67207374756666222c0a20202020227265666572656e636573223a205b0a2020202020207b0a2020202020202020224074797065223a20224f74686572222c0a2020202020202020226c6162656c223a202248617264666f726b20746f2050563130222c0a202020202020202022757269223a2022220a2020202020207d0a202020205d0a20207d2c0a202022617574686f7273223a205b0a202020207b0a202020202020226e616d65223a20224361726c6f73222c0a202020202020227769746e657373223a207b0a2020202020202020227769746e657373416c676f726974686d223a202265643235353139222c0a2020202020202020227075626c69634b6579223a202237656130396133346165626231336339383431633731333937623163616266656335646466393530343035323933646565343936636163326634333734383061222c0a2020202020202020227369676e6174757265223a20226134373639383562346363306434353766323437373937363131373939613666366138306663386362376563396463623561383232333838386430363138653330646531363566336438363963346130643931303764386135623631326164376335653432343431393037663562393137393666306437313837643634613031220a2020202020207d0a202020207d0a20205d0a7d"
}
|]

proposalMetaExpected :: ProposalMeta
proposalMetaExpected =
  ProposalMeta
    { _proposalMetaTxHash       = "257d75c8ddb0434e9b63e29ebb6241add2b835a307aa33aedba2effe09ed4ec8"
    , _proposalMetaCertIndex    = 2
    , _proposalMetaUrl          = "https://raw.githubusercontent.com/carloslodelar/proposals/main/pv10.json"
    , _proposalMetaHash         = "ffa226f3863aca006172d559cf46bb8b883a47233962ae2fc94c158d7de6fa81"
    , _proposalMetaJsonMetadata = object
        [ "body" .= object
            [ "title" .= ("Hardfork to Protocol version 10" :: Text)
            , "abstract" .= ("Let's have sanchoNet in full governance as soon as possible" :: Text)
            , "rationale" .= ("Let's keep testing stuff" :: Text)
            , "motivation" .= ("PV9 is not as fun as PV10" :: Text)
            , "references" .=
                [ object
                    [ "@type" .= ("Other" :: Text)
                    , "label" .= ("Hardfork to PV10" :: Text)
                    , "uri"   .= ("" :: Text)
                    ]
                ]
            ]
        , "authors" .=
            [ object
                [ "name" .= ("Carlos" :: Text)
                , "witness" .= object
                    [ "publicKey" .= ("7ea09a34aebb13c9841c71397b1cabfec5ddf950405293dee496cac2f437480a" :: Text)
                    , "signature" .= ("a476985b4cc0d457f247797611799a6f6a80fc8cb7ec9dcb5a8223888d0618e30de165f3d869c4a0d9107d8a5b612ad7c5e42441907f5b91796f0d7187d64a01" :: Text)
                    , "witnessAlgorithm" .= ("ed25519" :: Text)
                    ]
                ]
            ]
        , "@context" .= object
            [ "CIP100" .= ("https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#" :: Text)
            , "CIP108" .= ("https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#" :: Text)
            , "hashAlgorithm" .= ("CIP100:hashAlgorithm" :: Text)
            , "@language" .= ("en-us" :: Text)
            , "body" .= object
                [ "@id" .= ("CIP108:body" :: Text)
                , "@context" .= object
                    [ "title" .= ("CIP108:title" :: Text)
                    , "abstract" .= ("CIP108:abstract" :: Text)
                    , "rationale" .= ("CIP108:rationale" :: Text)
                    , "motivation" .= ("CIP108:motivation" :: Text)
                    , "references" .= object
                        [ "@id" .= ("CIP108:references" :: Text)
                        , "@container" .= ("@set" :: Text)
                        , "@context" .= object
                            [ "uri" .= ("CIP100:reference-uri" :: Text)
                            , "Other" .= ("CIP100:OtherReference" :: Text)
                            , "label" .= ("CIP100:reference-label" :: Text)
                            , "GovernanceMetadata" .= ("CIP100:GovernanceMetadataReference" :: Text)
                            , "referenceHash" .= object
                                [ "@id" .= ("CIP108:referenceHash" :: Text)
                                , "@context" .= object
                                    [ "hashDigest" .= ("CIP108:hashDigest" :: Text)
                                    , "hashAlgorithm" .= ("CIP100:hashAlgorithm" :: Text)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , "authors" .= object
                [ "@id" .= ("CIP100:authors" :: Text)
                , "@container" .= ("@set" :: Text)
                , "@context" .= object
                    [ "name" .= ("http://xmlns.com/foaf/0.1/name" :: Text)
                    , "witness" .= object
                        [ "@id" .= ("CIP100:witness" :: Text)
                        , "@context" .= object
                            [ "publicKey" .= ("CIP100:publicKey" :: Text)
                            , "signature" .= ("CIP100:signature" :: Text)
                            , "witnessAlgorithm" .= ("CIP100:witnessAlgorithm" :: Text)
                            ]
                        ]
                    ]
                ]
            ]
        , "hashAlgorithm" .= ("blake2b-256" :: Text)
        ]
    , _proposalMetaBytes        = "\\x7b0a20202240636f6e74657874223a207b0a2020202022406c616e6775616765223a2022656e2d7573222c0a2020202022434950313030223a202268747470733a2f2f6769746875622e636f6d2f63617264616e6f2d666f756e646174696f6e2f434950732f626c6f622f6d61737465722f4349502d303130302f524541444d452e6d6423222c0a2020202022434950313038223a202268747470733a2f2f6769746875622e636f6d2f63617264616e6f2d666f756e646174696f6e2f434950732f626c6f622f6d61737465722f4349502d303130382f524541444d452e6d6423222c0a202020202268617368416c676f726974686d223a20224349503130303a68617368416c676f726974686d222c0a2020202022626f6479223a207b0a20202020202022406964223a20224349503130383a626f6479222c0a2020202020202240636f6e74657874223a207b0a2020202020202020227265666572656e636573223a207b0a2020202020202020202022406964223a20224349503130383a7265666572656e636573222c0a202020202020202020202240636f6e7461696e6572223a202240736574222c0a202020202020202020202240636f6e74657874223a207b0a20202020202020202020202022476f7665726e616e63654d65746164617461223a20224349503130303a476f7665726e616e63654d657461646174615265666572656e6365222c0a202020202020202020202020224f74686572223a20224349503130303a4f746865725265666572656e6365222c0a202020202020202020202020226c6162656c223a20224349503130303a7265666572656e63652d6c6162656c222c0a20202020202020202020202022757269223a20224349503130303a7265666572656e63652d757269222c0a202020202020202020202020227265666572656e636548617368223a207b0a202020202020202020202020202022406964223a20224349503130383a7265666572656e636548617368222c0a20202020202020202020202020202240636f6e74657874223a207b0a202020202020202020202020202020202268617368446967657374223a20224349503130383a68617368446967657374222c0a202020202020202020202020202020202268617368416c676f726974686d223a20224349503130303a68617368416c676f726974686d220a20202020202020202020202020207d0a2020202020202020202020207d0a202020202020202020207d0a20202020202020207d2c0a2020202020202020227469746c65223a20224349503130383a7469746c65222c0a2020202020202020226162737472616374223a20224349503130383a6162737472616374222c0a2020202020202020226d6f7469766174696f6e223a20224349503130383a6d6f7469766174696f6e222c0a202020202020202022726174696f6e616c65223a20224349503130383a726174696f6e616c65220a2020202020207d0a202020207d2c0a2020202022617574686f7273223a207b0a20202020202022406964223a20224349503130303a617574686f7273222c0a2020202020202240636f6e7461696e6572223a202240736574222c0a2020202020202240636f6e74657874223a207b0a2020202020202020226e616d65223a2022687474703a2f2f786d6c6e732e636f6d2f666f61662f302e312f6e616d65222c0a2020202020202020227769746e657373223a207b0a2020202020202020202022406964223a20224349503130303a7769746e657373222c0a202020202020202020202240636f6e74657874223a207b0a202020202020202020202020227769746e657373416c676f726974686d223a20224349503130303a7769746e657373416c676f726974686d222c0a202020202020202020202020227075626c69634b6579223a20224349503130303a7075626c69634b6579222c0a202020202020202020202020227369676e6174757265223a20224349503130303a7369676e6174757265220a202020202020202020207d0a20202020202020207d0a2020202020207d0a202020207d0a20207d2c0a20202268617368416c676f726974686d223a2022626c616b6532622d323536222c0a202022626f6479223a207b0a20202020227469746c65223a202248617264666f726b20746f2050726f746f636f6c2076657273696f6e203130222c0a20202020226162737472616374223a20224c6574277320686176652073616e63686f4e657420696e2066756c6c20676f7665726e616e636520617320736f6f6e20617320706f737369626c65222c0a20202020226d6f7469766174696f6e223a2022505639206973206e6f742061732066756e2061732050563130222c0a2020202022726174696f6e616c65223a20224c65742773206b6565702074657374696e67207374756666222c0a20202020227265666572656e636573223a205b0a2020202020207b0a2020202020202020224074797065223a20224f74686572222c0a2020202020202020226c6162656c223a202248617264666f726b20746f2050563130222c0a202020202020202022757269223a2022220a2020202020207d0a202020205d0a20207d2c0a202022617574686f7273223a205b0a202020207b0a202020202020226e616d65223a20224361726c6f73222c0a202020202020227769746e657373223a207b0a2020202020202020227769746e657373416c676f726974686d223a202265643235353139222c0a2020202020202020227075626c69634b6579223a202237656130396133346165626231336339383431633731333937623163616266656335646466393530343035323933646565343936636163326634333734383061222c0a2020202020202020227369676e6174757265223a20226134373639383562346363306434353766323437373937363131373939613666366138306663386362376563396463623561383232333838386430363138653330646531363566336438363963346130643931303764386135623631326164376335653432343431393037663562393137393666306437313837643634613031220a2020202020207d0a202020207d0a20205d0a7d"
    }
