-- | Responses for Cardano governance queries

module Blockfrost.Types.Cardano.Governance
  ( DRep (..)
  , DRepInfo (..)
  , DRepDelegator (..)
  , DRepMeta (..)
  , DRepRegistrationAction (..)
  , DRepUpdate (..)
  , VotingAction (..)
  , DRepVote (..)
  , ProposalAction (..)
  , Proposal (..)
  , ProposalInfo (..)
  , ProposedProtocolParams (..)
  , ParamProposal (..)
  , WithdrawalProposal (..)
  , VoterRole (..)
  , ProposalVote (..)
  , ProposalMeta (..)
  ) where

import Blockfrost.Types.Shared
import Blockfrost.Types.Cardano.Epochs (CostModels, CostModelsRaw)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.=), object, withText)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples, singleSample)

-- | DRep index
data DRep = DRep
  { _dRepDrepId :: DRepIdBech32 -- ^ Bech32 encoded DRep address
  , _dRepHex    :: DRepIdHex    -- ^ The raw bytes of the DRep
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_dRep", CamelToSnake]] DRep

instance ToSample DRep where
    toSamples = pure $ samples
      [ DRep
          { _dRepDrepId = "drep1mvdu8slennngja7w4un6knwezufra70887zuxpprd64jxfveahn"
          , _dRepHex    = "db1bc3c3f99ce68977ceaf27ab4dd917123ef9e73f85c304236eab23"
          }
      , DRep
          { _dRepDrepId = "drep1cxayn4fgy27yaucvhamsvqj3v6835mh3tjjx6x8hdnr4"
          , _dRepHex    = "c1ba49d52822bc4ef30cbf77060251668f1a6ef15ca46d18f76cc758"
          }
      ]

-- | Information about a DRep
data DRepInfo = DRepInfo
  { _dRepInfoDrepId          :: DRepIdBech32 -- ^ Bech32 encoded DRep address
  , _dRepInfoHex             :: DRepIdHex    -- ^ The raw bytes of the DRep
  , _dRepInfoAmount          :: Lovelaces    -- ^ The total amount of voting power this DRep is delegated
  , _dRepInfoHasScript       :: Bool         -- ^ Flag which indicates if this DRep credentials are a script hash
  , _dRepInfoRetired         :: Bool         -- ^ Indicates the registration state of the DRep. Set to `True` if the DRep has been deregistered; otherwise, `False`.
  , _dRepInfoExpired         :: Bool         -- ^ Indicates whether the DRep has been inactive for a consecutive number of epochs (determined by a epoch parameter `drep_activity`)
  , _dRepInfoLastActiveEpoch :: Maybe Epoch  -- ^ Epoch of the most recent action - registration, update, deregistration or voting
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_dRepInfo", CamelToSnake]] DRepInfo

instance ToSample DRepInfo where
  toSamples = pure $ singleSample $ DRepInfo
    { _dRepInfoDrepId          = "drep15cfxz9exyn5rx0807zvxfrvslrjqfchrd4d47kv9e0f46uedqtc"
    , _dRepInfoHex             = "a61261172624e8333ceff098648d90f8e404e2e36d5b5f5985cbd35d"
    , _dRepInfoAmount          = 2000000
    , _dRepInfoHasScript       = True
    , _dRepInfoRetired         = False
    , _dRepInfoExpired         = False
    , _dRepInfoLastActiveEpoch = Just 509
    }

-- | Address delegating to a DRep
data DRepDelegator = DRepDelegator
  { _dRepDelegatorAddress :: Address   -- ^ Bech32 encoded stake addresses
  , _dRepDelegatorAmount  :: Lovelaces -- ^ Currently delegated amount
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_dRepDelegator", CamelToSnake]] DRepDelegator

instance ToSample DRepDelegator where
  toSamples = pure $ samples
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

-- | DRep metadata
data DRepMeta = DRepMeta
  { _dRepMetaDrepId          :: DRepIdBech32 -- ^ Bech32 encoded DRep address
  , _dRepMetaHex             :: DRepIdHex    -- ^ The raw bytes of the DRep
  , _dRepMetaUrl             :: Text         -- ^ URL to the drep metadata
  , _dRepMetaHash            :: Text         -- ^ Hash of the metadata file
  , _dRepMetaJsonMetadata    :: Value        -- ^ Content of the JSON metadata (validated CIP-119)
  , _dRepMetaBytes           :: Text         -- ^ Hex-encoded metadata (raw)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_dRepMeta", CamelToSnake]] DRepMeta

instance ToSample DRepMeta where
  toSamples = pure $ singleSample $ DRepMeta
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

-- | DRep registration action
data DRepRegistrationAction
  = DRepRegistrationAction_Registered
  | DRepRegistrationAction_Deregistered
  deriving stock (Show, Eq, Generic)

instance ToJSON DRepRegistrationAction where
  toJSON DRepRegistrationAction_Registered   = toJSON ("registered"   :: Text)
  toJSON DRepRegistrationAction_Deregistered = toJSON ("deregistered" :: Text)

  toEncoding DRepRegistrationAction_Registered   = toEncoding ("registered"   :: Text)
  toEncoding DRepRegistrationAction_Deregistered = toEncoding ("deregistered" :: Text)

instance FromJSON DRepRegistrationAction where
  parseJSON = withText "action" $ \case
    "registered"   -> pure DRepRegistrationAction_Registered
    "deregistered" -> pure DRepRegistrationAction_Deregistered
    x              -> fail ("Expected registration action got " ++ show x)

-- | DRep registration update
data DRepUpdate = DRepUpdate
  { _dRepUpdateTxHash    :: TxHash                 -- ^ Transaction ID
  , _dRepUpdateCertIndex :: Integer                -- ^ Index of the certificate within the update transaction
  , _dRepUpdateAction    :: DRepRegistrationAction -- ^ DRep registration action
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_dRepUpdate", CamelToSnake]] DRepUpdate

instance ToSample DRepUpdate where
  toSamples = pure $ samples
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

-- | DRep voting action
data VotingAction
  = VotingAction_Yes
  | VotingAction_No
  | VotingAction_Abstain
  deriving stock (Show, Eq, Generic)

instance ToJSON VotingAction where
  toJSON VotingAction_Yes     = toJSON ("yes"     :: Text)
  toJSON VotingAction_No      = toJSON ("no"      :: Text)
  toJSON VotingAction_Abstain = toJSON ("abstain" :: Text)

  toEncoding VotingAction_Yes     = toEncoding ("yes"     :: Text)
  toEncoding VotingAction_No      = toEncoding ("no"      :: Text)
  toEncoding VotingAction_Abstain = toEncoding ("abstain" :: Text)

instance FromJSON VotingAction where
  parseJSON = withText "vote" $ \case
    "yes"     -> pure VotingAction_Yes
    "no"      -> pure VotingAction_No
    "abstain" -> pure VotingAction_Abstain
    x              -> fail ("Expected DRep vote but got " ++ show x)

-- | DRep vote
data DRepVote = DRepVote
  { _dRepVoteTxHash    :: TxHash       -- ^ Transaction ID
  , _dRepVoteCertIndex :: Integer      -- ^ Index of the certificate within the update transaction
  , _dRepVoteAction    :: VotingAction -- ^ DRep voting action
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_dRepVote", CamelToSnake, Rename "action" "vote"]] DRepVote

instance ToSample DRepVote where
  toSamples = pure $ samples
    [ DRepVote
        { _dRepVoteTxHash    = "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5"
        , _dRepVoteCertIndex = 2
        , _dRepVoteAction    = VotingAction_Yes
        }
    , DRepVote
        { _dRepVoteTxHash    = "b302de601defdf11a5261ed31a263804dac4a582a888c998ce24dec5"
        , _dRepVoteCertIndex = 3
        , _dRepVoteAction    = VotingAction_Abstain
        }
    ]

-- | DRep proposal action
data ProposalAction
  = ProposalAction_HardForkInitiation
  | ProposalAction_NewCommittee
  | ProposalAction_NewConstitution
  | ProposalAction_InfoAction
  | ProposalAction_NoConfidence
  | ProposalAction_ParameterChange
  | ProposalAction_TreasuryWithdrawals
  deriving stock (Show, Eq, Generic)

instance ToJSON ProposalAction where
  toJSON ProposalAction_HardForkInitiation  = toJSON ("hard_fork_initiation" :: Text)
  toJSON ProposalAction_NewCommittee        = toJSON ("new_committee"        :: Text)
  toJSON ProposalAction_NewConstitution     = toJSON ("new_constitution"     :: Text)
  toJSON ProposalAction_InfoAction          = toJSON ("info_action"          :: Text)
  toJSON ProposalAction_NoConfidence        = toJSON ("no_confidence"        :: Text)
  toJSON ProposalAction_ParameterChange     = toJSON ("parameter_change"     :: Text)
  toJSON ProposalAction_TreasuryWithdrawals = toJSON ("treasury_withdrawals" :: Text)

  toEncoding ProposalAction_HardForkInitiation  = toEncoding ("hard_fork_initiation" :: Text)
  toEncoding ProposalAction_NewCommittee        = toEncoding ("new_committee"        :: Text)
  toEncoding ProposalAction_NewConstitution     = toEncoding ("new_constitution"     :: Text)
  toEncoding ProposalAction_InfoAction          = toEncoding ("info_action"          :: Text)
  toEncoding ProposalAction_NoConfidence        = toEncoding ("no_confidence"        :: Text)
  toEncoding ProposalAction_ParameterChange     = toEncoding ("parameter_change"     :: Text)
  toEncoding ProposalAction_TreasuryWithdrawals = toEncoding ("treasury_withdrawals" :: Text)

instance FromJSON ProposalAction where
  parseJSON = withText "proposal" $ \case
    "hard_fork_initiation" -> pure ProposalAction_HardForkInitiation
    "new_committee"        -> pure ProposalAction_NewCommittee
    "new_constitution"     -> pure ProposalAction_NewConstitution
    "info_action"          -> pure ProposalAction_InfoAction
    "no_confidence"        -> pure ProposalAction_NoConfidence
    "parameter_change"     -> pure ProposalAction_ParameterChange
    "treasury_withdrawals" -> pure ProposalAction_TreasuryWithdrawals
    x              -> fail ("Expected DRep proposal but got " ++ show x)

-- | DRep proposal
data Proposal = Proposal
  { _dRepProposalTxHash    :: TxHash             -- ^ Transaction ID
  , _dRepProposalCertIndex :: Integer            -- ^ Index of the certificate within the update transaction
  , _dRepProposalAction    :: ProposalAction -- ^ DRep proposal type
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_dRepProposal", CamelToSnake, Rename "action" "governance_type"]] Proposal

instance ToSample Proposal where
  toSamples = pure $ samples
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

-- | Proposal details
data ProposalInfo = ProposalInfo
  { _proposalInfoTxHash                :: TxHash             -- ^ Transaction ID
  , _proposalInfoCertIndex             :: Integer            -- ^ Index of the certificate within the proposal transaction
  , _proposalInfoGovernanceType        :: ProposalAction     -- ^ DRep proposal type
  , _proposalInfoDeposit               :: Lovelaces          -- ^ The deposit amount paid for this proposal
  , _proposalInfoReturnAddress         :: Address            -- ^ Bech32 stake address of the reward address to receive the deposit when it is repaid.
  , _proposalInfoGovernanceDescription :: Maybe Value        -- ^ An object describing the content of this GovActionProposal in a readable way.
  , _proposalInfoRatifiedEpoch         :: Maybe Epoch
  , _proposalInfoEnactedEpoch          :: Maybe Epoch
  , _proposalInfoDroppedEpoch          :: Maybe Epoch
  , _proposalInfoExpiredEpoch          :: Maybe Epoch
  , _proposalInfoExpiration            :: Epoch              -- ^ The epoch at which this governance action will expire.
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_proposalInfo", CamelToSnake]] ProposalInfo

instance ToSample ProposalInfo where
  toSamples = pure $ singleSample $ ProposalInfo
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

-- | Proposed protocol parameters
--
-- This type is sturcturally similar to @ProtocolParams@ but every field
-- is optional.
data ProposedProtocolParams = ProposedProtocolParams
  { _proposedProtocolParamsEpoch                      :: Maybe Epoch -- ^ Epoch number
  , _proposedProtocolParamsMinFeeA                    :: Maybe Integer -- ^ The linear factor for the minimum fee calculation for given epoch
  , _proposedProtocolParamsMinFeeB                    :: Maybe Integer -- ^ The constant factor for the minimum fee calculation
  , _proposedProtocolParamsMaxBlockSize               :: Maybe Integer -- ^ Maximum block body size in Bytes
  , _proposedProtocolParamsMaxTxSize                  :: Maybe Integer -- ^ Maximum transaction size
  , _proposedProtocolParamsMaxBlockHeaderSize         :: Maybe Integer -- ^ Maximum block header size
  , _proposedProtocolParamsKeyDeposit                 :: Maybe Lovelaces -- ^ The amount of a key registration deposit in Lovelaces
  , _proposedProtocolParamsPoolDeposit                :: Maybe Lovelaces -- ^ The amount of a pool registration deposit in Lovelaces
  , _proposedProtocolParamsEMax                       :: Maybe Integer -- ^ Epoch bound on pool retirement
  , _proposedProtocolParamsNOpt                       :: Maybe Integer -- ^ Desired number of pools
  , _proposedProtocolParamsA0                         :: Maybe Rational -- ^ Pool pledge influence
  , _proposedProtocolParamsRho                        :: Maybe Rational -- ^ Monetary expansion
  , _proposedProtocolParamsTau                        :: Maybe Rational -- ^ Treasury expansion
  , _proposedProtocolParamsDecentralisationParam      :: Maybe Rational -- ^ Percentage of blocks produced by federated nodes
  , _proposedProtocolParamsExtraEntropy               :: Maybe Text -- ^ Seed for extra entropy
  , _proposedProtocolParamsProtocolMajorVer           :: Maybe Integer -- ^ Accepted protocol major version
  , _proposedProtocolParamsProtocolMinorVer           :: Maybe Integer -- ^ Accepted protocol minor version
  , _proposedProtocolParamsMinUtxo                    :: Maybe Lovelaces -- ^ Minimum UTXO value
  , _proposedProtocolParamsMinPoolCost                :: Maybe Lovelaces  -- ^ Minimum stake cost forced on the pool
  , _proposedProtocolParamsNonce                      :: Maybe Text -- ^ Epoch number only used once
  , _proposedProtocolParamsCostModels                 :: Maybe CostModels -- ^ Cost models parameters for Plutus Core scripts
  , _proposedProtocolParamsCostModelsRaw              :: Maybe CostModelsRaw
  , _proposedProtocolParamsPriceMem                   :: Maybe Rational -- ^ The per word cost of script memory usage
  , _proposedProtocolParamsPriceStep                  :: Maybe Rational -- ^ The cost of script execution step usage
  , _proposedProtocolParamsMaxTxExMem                 :: Maybe Quantity -- ^ The maximum number of execution memory allowed to be used in a single transaction
  , _proposedProtocolParamsMaxTxExSteps               :: Maybe Quantity -- ^ The maximum number of execution steps allowed to be used in a single transaction
  , _proposedProtocolParamsMaxBlockExMem              :: Maybe Quantity -- ^ The maximum number of execution memory allowed to be used in a single block
  , _proposedProtocolParamsMaxBlockExSteps            :: Maybe Quantity -- ^ The maximum number of execution steps allowed to be used in a single block
  , _proposedProtocolParamsMaxValSize                 :: Maybe Quantity -- ^ The maximum Val size
  , _proposedProtocolParamsCollateralPercent          :: Maybe Integer -- ^ The percentage of the transactions fee which must be provided as collateral when including non-native scripts
  , _proposedProtocolParamsMaxCollateralInputs        :: Maybe Integer -- ^ The maximum number of collateral inputs allowed in a transaction
  , _proposedProtocolParamsCoinsPerUtxoSize           :: Maybe Lovelaces -- ^ The cost per UTxO size. Cost per UTxO *word* for Alozno. Cost per UTxO *byte* for Babbage and later
  , _proposedProtocolParamsCoinsPerUtxoWord           :: Maybe Lovelaces -- ^ The cost per UTxO word (DEPRECATED)
  , _proposedProtocolParamsPvtMotionNoConfidence      :: Maybe Rational
  , _proposedProtocolParamsPvtCommitteeNormal         :: Maybe Rational
  , _proposedProtocolParamsPvtCommitteeNoConfidence   :: Maybe Rational
  , _proposedProtocolParamsPvtHardForkInitiation      :: Maybe Rational
  , _proposedProtocolParamsPvtppSecurityGroup         :: Maybe Rational
  , _proposedProtocolParamsDvtMotionNoConfidence      :: Maybe Rational
  , _proposedProtocolParamsDvtCommitteeNormal         :: Maybe Rational
  , _proposedProtocolParamsDvtCommitteeNoConfidence   :: Maybe Rational
  , _proposedProtocolParamsDvtUpdateToConstitution    :: Maybe Rational
  , _proposedProtocolParamsDvtHardForkInitiation      :: Maybe Rational
  , _proposedProtocolParamsDvtPPNetworkGroup          :: Maybe Rational
  , _proposedProtocolParamsDvtPPEconomicGroup         :: Maybe Rational
  , _proposedProtocolParamsDvtPPTechnicalGroup        :: Maybe Rational
  , _proposedProtocolParamsDvtPPGovGroup              :: Maybe Rational
  , _proposedProtocolParamsDvtTreasuryWithdrawal      :: Maybe Rational
  , _proposedProtocolParamsCommitteeMinSize           :: Maybe Quantity
  , _proposedProtocolParamsCommitteeMaxTermLength     :: Maybe Quantity
  , _proposedProtocolParamsGovActionLifetime          :: Maybe Quantity
  , _proposedProtocolParamsGovActionDeposit           :: Maybe Lovelaces
  , _proposedProtocolParamsDrepDeposit                :: Maybe Lovelaces
  , _proposedProtocolParamsDrepActivity               :: Maybe Quantity
  , _proposedProtocolParamsMinFeeRefScriptCostPerByte :: Maybe Rational
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_proposedProtocolParams", CamelToSnake, Rename "dvt_pp_network_group" "dvt_p_p_network_group", Rename "dvt_pp_economic_group" "dvt_p_p_economic_group", Rename "dvt_pp_technical_group" "dvt_p_p_technical_group", Rename "dvt_pp_gov_group" "dvt_p_p_gov_group"]] ProposedProtocolParams

instance ToSample ProposedProtocolParams where
  toSamples = pure $ singleSample
    ProposedProtocolParams
      { _proposedProtocolParamsEpoch = Just 225
      , _proposedProtocolParamsMinFeeA = Just 44
      , _proposedProtocolParamsMinFeeB = Just 155381
      , _proposedProtocolParamsMaxBlockSize = Just 65536
      , _proposedProtocolParamsMaxTxSize = Just 16384
      , _proposedProtocolParamsMaxBlockHeaderSize = Just 1100
      , _proposedProtocolParamsKeyDeposit = Just 2000000
      , _proposedProtocolParamsPoolDeposit = Just 500000000
      , _proposedProtocolParamsEMax = Just 18
      , _proposedProtocolParamsNOpt = Just 150
      , _proposedProtocolParamsA0 = Just 0.3
      , _proposedProtocolParamsRho = Just 0.003
      , _proposedProtocolParamsTau = Just 0.2
      , _proposedProtocolParamsDecentralisationParam = Just 0.5
      , _proposedProtocolParamsExtraEntropy = Nothing
      , _proposedProtocolParamsProtocolMajorVer = Just 2
      , _proposedProtocolParamsProtocolMinorVer = Just 0
      , _proposedProtocolParamsMinUtxo = Just 1000000
      , _proposedProtocolParamsMinPoolCost = Just 340000000
      , _proposedProtocolParamsNonce = Just "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81"
      , _proposedProtocolParamsCostModels =
          case toSamples (Proxy :: Proxy CostModels) of
            [(_, pp)] -> Just pp
            _ -> error "Absurd"
      , _proposedProtocolParamsCostModelsRaw =
          case toSamples (Proxy :: Proxy CostModelsRaw) of
            [(_, pp)] -> Just pp
            _ -> error "Absurd"
      , _proposedProtocolParamsPriceMem = Just 0.0577
      , _proposedProtocolParamsPriceStep = Just 0.0000721
      , _proposedProtocolParamsMaxTxExMem = Just 10000000
      , _proposedProtocolParamsMaxTxExSteps = Just 10000000000
      , _proposedProtocolParamsMaxBlockExMem = Just 50000000
      , _proposedProtocolParamsMaxBlockExSteps = Just 40000000000
      , _proposedProtocolParamsMaxValSize = Just 5000
      , _proposedProtocolParamsCollateralPercent = Just 150
      , _proposedProtocolParamsMaxCollateralInputs = Just 3
      , _proposedProtocolParamsCoinsPerUtxoSize = Just 34482
      , _proposedProtocolParamsCoinsPerUtxoWord = Just 34482
      , _proposedProtocolParamsPvtMotionNoConfidence = Just 0.51
      , _proposedProtocolParamsPvtCommitteeNormal = Just 0.51
      , _proposedProtocolParamsPvtCommitteeNoConfidence = Just 0.51
      , _proposedProtocolParamsPvtHardForkInitiation = Just 0.51
      , _proposedProtocolParamsPvtppSecurityGroup = Just 0.51
      , _proposedProtocolParamsDvtMotionNoConfidence = Just 0.67
      , _proposedProtocolParamsDvtCommitteeNormal = Just 0.67
      , _proposedProtocolParamsDvtCommitteeNoConfidence = Just 0.6
      , _proposedProtocolParamsDvtUpdateToConstitution = Just 0.75
      , _proposedProtocolParamsDvtHardForkInitiation = Just 0.6
      , _proposedProtocolParamsDvtPPNetworkGroup = Just 0.67
      , _proposedProtocolParamsDvtPPEconomicGroup = Just 0.67
      , _proposedProtocolParamsDvtPPTechnicalGroup = Just 0.67
      , _proposedProtocolParamsDvtPPGovGroup = Just 0.75
      , _proposedProtocolParamsDvtTreasuryWithdrawal = Just 0.67
      , _proposedProtocolParamsCommitteeMinSize = Just 7
      , _proposedProtocolParamsCommitteeMaxTermLength = Just 146
      , _proposedProtocolParamsGovActionLifetime = Just 6
      , _proposedProtocolParamsGovActionDeposit = Just 100000000000
      , _proposedProtocolParamsDrepDeposit = Just 500000000
      , _proposedProtocolParamsDrepActivity = Just 20
      , _proposedProtocolParamsMinFeeRefScriptCostPerByte = Just 15
      }

-- | Parameter proposal details
data ParamProposal = ParamProposal
  { _paramProposalTxHash     :: TxHash                 -- ^ Transaction ID
  , _paramProposalCertIndex  :: Integer                -- ^ Index of the certificate within the proposal transaction
  , _paramProposalParameters :: ProposedProtocolParams -- ^ Proposed parameters
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_paramProposal", CamelToSnake]] ParamProposal

instance ToSample ParamProposal where
  toSamples = pure $ singleSample $ ParamProposal
    { _paramProposalTxHash     = "2dd15e0ef6e6a17841cb9541c27724072ce4d4b79b91e58432fbaa32d9572531"
    , _paramProposalCertIndex  = 2
    , _paramProposalParameters =
        case toSamples (Proxy :: Proxy ProposedProtocolParams) of
          [(_, pp)] -> pp
          _ -> error "Absurd"
    }

-- | Withdrawal proposal details
data WithdrawalProposal = WithdrawalProposal
  { _withdrawalProposalStakeAddress :: Address    -- ^ Bech32 stake address
  , _withdrawalProposalAmount       :: Lovelaces -- ^ Proposed withdrawal amount
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_withdrawalProposal", CamelToSnake]] WithdrawalProposal

instance ToSample WithdrawalProposal where
  toSamples = pure $ samples
    [ WithdrawalProposal
        { _withdrawalProposalStakeAddress = "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7"
        , _withdrawalProposalAmount       = 454541212442
        }
    , WithdrawalProposal
        { _withdrawalProposalStakeAddress = "stake1xx2g2c9dx2nhhehyrezyxpkstoppcqmu9hk63qgfkccw5rqttygt2"
        , _withdrawalProposalAmount       = 97846969
        }
    ]

data VoterRole
  = VoterRole_ConstitutionalCommittee
  | VoterRole_DRep
  | VoterRole_SPO
  deriving stock (Show, Eq, Generic)

instance ToJSON VoterRole where
  toJSON VoterRole_ConstitutionalCommittee = toJSON ("constitutional_committee" :: Text)
  toJSON VoterRole_DRep                    = toJSON ("drep"                     :: Text)
  toJSON VoterRole_SPO                     = toJSON ("spo"                      :: Text)

  toEncoding VoterRole_ConstitutionalCommittee = toEncoding ("constitutional_committee" :: Text)
  toEncoding VoterRole_DRep                    = toEncoding ("drep"                     :: Text)
  toEncoding VoterRole_SPO                     = toEncoding ("spo"                      :: Text)

instance FromJSON VoterRole where
  parseJSON = withText "voterRole" $ \case
    "constitutional_committee" -> pure VoterRole_ConstitutionalCommittee
    "drep"                     -> pure VoterRole_DRep
    "spo"                      -> pure VoterRole_SPO
    x              -> fail ("Expected DRep vote but got " ++ show x)

-- | Proposal vote
data ProposalVote = ProposalVote
  { _proposalVoteTxHash    :: TxHash       -- ^ Transaction ID
  , _proposalVoteCertIndex :: Integer      -- ^ Index of the certificate within the proposal transaction
  , _proposalVoteVoterRole :: VoterRole    -- ^ The role of the voter
  , _proposalVoteVoter     :: DRepId       -- ^ The actual voter
  , _proposalVoteAction    :: VotingAction -- ^ Actual voting action
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_proposalVote", CamelToSnake, Rename "action" "vote"]] ProposalVote

instance ToSample ProposalVote where
  toSamples = pure $ samples
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

-- | Prposal metadata
data ProposalMeta = ProposalMeta
  { _proposalMetaTxHash       :: TxHash  -- ^ Transaction ID
  , _proposalMetaCertIndex    :: Integer -- ^ Index of the certificate within the proposal transaction
  , _proposalMetaUrl          :: Text    -- ^ URL to the proposal metadata
  , _proposalMetaHash         :: Text    -- ^ Hash of the metadata file
  , _proposalMetaJsonMetadata :: Value   -- ^ Content of the JSON metadata (validated CIP-108)
  , _proposalMetaBytes        :: Text    -- ^ Hex-encoded metadata (raw)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_proposalMeta", CamelToSnake]] ProposalMeta

instance ToSample ProposalMeta where
  toSamples = pure $ singleSample $ ProposalMeta
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
