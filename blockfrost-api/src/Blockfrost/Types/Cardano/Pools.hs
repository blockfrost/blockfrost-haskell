-- | Cardano Pools reponses

module Blockfrost.Types.Cardano.Pools
  ( PoolEpoch (..)
  , PoolInfo (..)
  , PoolHistory (..)
  , PoolMetadata (..)
  , PoolRelay (..)
  , PoolDelegator (..)
  , PoolUpdate (..)
  , PoolRegistrationAction (..)
  , samplePoolRelay
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, pairs, withText)
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples, singleSample)

import Blockfrost.Types.Shared

-- | Retirement epoch for pool
data PoolEpoch = PoolEpoch
  { _poolEpochPoolId :: PoolId -- ^ Bech32 encoded pool ID
  , _poolEpochEpoch  :: Epoch -- ^ Retirement epoch number
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolEpoch", CamelToSnake]] PoolEpoch

instance ToSample PoolEpoch where
  toSamples = pure $ samples
    [ PoolEpoch "pool19u64770wqp6s95gkajc8udheske5e6ljmpq33awxk326zjaza0q" 225
    , PoolEpoch "pool1dvla4zq98hpvacv20snndupjrqhuc79zl6gjap565nku6et5zdx" 215
    , PoolEpoch "pool1wvccajt4eugjtf3k0ja3exjqdj7t8egsujwhcw4tzj4rzsxzw5w" 231
    ]

-- | Detailed pool information
data PoolInfo = PoolInfo
  { _poolInfoPoolId         :: PoolId -- ^ Bech32 encoded pool ID
  , _poolInfoHex            :: Text -- ^ Hexadecimal pool ID.
  , _poolInfoVrfKey         :: Text -- ^ VRF key hash
  , _poolInfoBlocksMinted   :: Integer -- ^ Total minted blocks
  , _poolInfoBlocksEpoch    :: Integer -- ^ Number of blocks minted in the current epoch
  , _poolInfoLiveStake      :: Lovelaces
  , _poolInfoLiveSize       :: Double
  , _poolInfoLiveSaturation :: Double
  , _poolInfoLiveDelegators :: Double
  , _poolInfoActiveStake    :: Lovelaces
  , _poolInfoActiveSize     :: Double
  , _poolInfoDeclaredPledge :: Lovelaces -- ^ Stake pool certificate pledge
  , _poolInfoLivePledge     :: Lovelaces -- ^ Stake pool current pledge
  , _poolInfoMarginCost     :: Rational -- ^ Margin tax cost of the stake pool
  , _poolInfoFixedCost      :: Lovelaces -- ^ Fixed tax cost of the stake pool
  , _poolInfoRewardAccount  :: Address -- ^ Bech32 reward account of the stake pool
  , _poolInfoOwners         :: [Address]
  , _poolInfoRegistration   :: [Text]
  , _poolInfoRetirement     :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolInfo", CamelToSnake]] PoolInfo

instance ToSample PoolInfo where
  toSamples = pure $ singleSample
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

-- | History of a stake pool parameters over epochs
data PoolHistory = PoolHistory
  { _poolHistoryEpoch           :: Epoch -- ^ Epoch number
  , _poolHistoryBlocks          :: Integer -- ^ Number of blocks created by pool
  , _poolHistoryActiveStake     :: Lovelaces -- ^ Active (Snapshot of live stake 2 epochs ago) stake in Lovelaces
  , _poolHistoryActiveSize      :: Double -- ^ Pool size (percentage) of overall active stake at that epoch
  , _poolHistoryDelegatorsCount :: Integer -- ^ Number of delegators for epoch
  , _poolHistoryRewards         :: Lovelaces -- ^ Total rewards received before distribution to delegators
  , _poolHistoryFees            :: Lovelaces -- ^ Pool operator rewards
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolHistory", CamelToSnake]] PoolHistory

instance ToSample PoolHistory where
  toSamples = pure $ singleSample
    PoolHistory
      { _poolHistoryEpoch = 233
      , _poolHistoryBlocks = 22
      , _poolHistoryActiveStake = 20485965693569
      , _poolHistoryActiveSize = 1.2345
      , _poolHistoryDelegatorsCount = 115
      , _poolHistoryRewards = 206936253674159
      , _poolHistoryFees = 1290968354
      }

-- | Stake pool registration metadata
data PoolMetadata = PoolMetadata
  { _poolMetadataPoolId      :: PoolId -- ^ Bech32 pool ID
  , _poolMetadataHex         :: Text -- ^ Hexadecimal pool ID
  , _poolMetadataUrl         :: Maybe Text -- ^ URL to the stake pool metadata
  , _poolMetadataHash        :: Maybe Text -- ^ Hash of the metadata file
  , _poolMetadataTicker      :: Maybe Text -- ^ Ticker of the stake pool
  , _poolMetadataName        :: Maybe Text -- ^ Name of the stake pool
  , _poolMetadataDescription :: Maybe Text -- ^ Description of the stake pool
  , _poolMetadataHomepage    :: Maybe Text -- ^ Home page of the stake pool
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolMetadata", CamelToSnake]] PoolMetadata

-- We need this more specific
-- instance since API returns
-- empty object if there's no metadata
instance {-# OVERLAPS #-} ToJSON (Maybe PoolMetadata) where
  toJSON Nothing   = object mempty
  toJSON (Just pm) = toJSON pm
  toEncoding Nothing   = pairs mempty
  toEncoding (Just pm) = toEncoding pm
instance {-# OVERLAPS #-} FromJSON (Maybe PoolMetadata) where
  parseJSON x | x == object [] = pure Nothing
  parseJSON x = Just <$> parseJSON x

instance ToSample PoolMetadata where
  toSamples = pure $ singleSample samplePoolMetadata

samplePoolMetadata :: PoolMetadata
samplePoolMetadata =
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

-- | Relays of a stake pool
data PoolRelay = PoolRelay
  { _poolRelayIpv4   :: Maybe Text -- ^ IPv4 address of the relay
  , _poolRelayIpv6   :: Maybe Text -- ^ IPv6 address of the relay
  , _poolRelayDns    :: Maybe Text -- ^ DNS name of the relay
  , _poolRelayDnsSrv :: Maybe Text -- ^ DNS SRV entry of the relay
  , _poolRelayPort   :: Integer -- ^ Network port of the relay
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolRelay", CamelToSnake]] PoolRelay

instance ToSample PoolRelay where
  toSamples = pure $ singleSample samplePoolRelay

-- | Example of `PoolRelay`
samplePoolRelay :: PoolRelay
samplePoolRelay =
  PoolRelay
    { _poolRelayIpv4 = Just "4.4.4.4"
    , _poolRelayIpv6 = Just "https://stakenuts.com/mainnet.json"
    , _poolRelayDns = Just "relay1.stakenuts.com"
    , _poolRelayDnsSrv = Just "_relays._tcp.relays.stakenuts.com"
    , _poolRelayPort = 3001
    }

-- | Stake pool delegator
data PoolDelegator = PoolDelegator
  { _poolDelegatorAddress   :: Text -- ^ Bech32 encoded stake addresses
  , _poolDelegatorLiveStake :: Lovelaces -- ^ Currently delegated amount
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolDelegator", CamelToSnake]] PoolDelegator

instance ToSample PoolDelegator where
  toSamples = pure $ samples
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

-- | Registration action of a pool
data PoolRegistrationAction = PoolRegistered | PoolDeregistered
  deriving stock (Show, Eq, Generic)

instance ToJSON PoolRegistrationAction where
  toJSON PoolRegistered   = toJSON ("registered" :: Text)
  toJSON PoolDeregistered = toJSON ("deregistered" :: Text)
  toEncoding PoolRegistered   = toEncoding ("registered" :: Text)
  toEncoding PoolDeregistered = toEncoding ("deregistered" :: Text)

instance FromJSON PoolRegistrationAction where
  parseJSON = withText "action" $ \case
    "registered"   -> pure PoolRegistered
    "deregistered" -> pure PoolDeregistered
    x              -> fail ("Expected registration action got " ++ show x)

instance ToSample PoolRegistrationAction where
  toSamples = pure $ samples [ PoolRegistered, PoolDeregistered ]

-- | Certificate update to the stake pool
data PoolUpdate = PoolUpdate
  { _poolUpdateTxHash    :: TxHash -- ^ Transaction ID
  , _poolUpdateCertIndex :: Integer -- ^ Certificate within the transaction
  , _poolUpdateAction    :: PoolRegistrationAction -- ^ Action in the certificate
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolUpdate", CamelToSnake]] PoolUpdate

instance ToSample PoolUpdate where
  toSamples = pure $ samples
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
