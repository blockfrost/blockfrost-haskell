-- | Responses for Cardano epoch quries

module Blockfrost.Types.Cardano.Epochs
  ( EpochInfo (..)
  , PoolStakeDistribution (..)
  , ProtocolParams (..)
  , StakeDistribution (..)
  ) where

import Blockfrost.Types.Shared
import Data.Text (Text)
import Deriving.Aeson
import Servant.Docs (ToSample (..), singleSample)

-- | Information about an epoch
data EpochInfo = EpochInfo
  { _epochInfoEpoch          :: Epoch -- ^ Epoch number
  , _epochInfoStartTime      :: POSIXTime -- ^ Unix time of the start of the epoch
  , _epochInfoEndTime        :: POSIXTime -- ^ Unix time of the end of the epoch
  , _epochInfoFirstBlockTime :: POSIXTime -- ^ Unix time of the first block of the epoch
  , _epochInfoLastBlockTime  :: POSIXTime -- ^ Unix time of the last block of the epoch
  , _epochInfoBlockCount     :: Integer -- ^ Number of blocks within the epoch
  , _epochInfoTxCount        :: Integer -- ^ Number of transactions within the epoch
  , _epochInfoOutput         :: Lovelaces -- ^ Sum of all the transactions within the epoch in Lovelaces
  , _epochInfoFees           :: Lovelaces -- ^ Sum of all the fees within the epoch in Lovelaces
  , _epochInfoActiveStake    :: Maybe Lovelaces -- ^ Sum of all the active stakes within the epoch in Lovelaces
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_epochInfo", CamelToSnake]] EpochInfo

instance ToSample EpochInfo where
  toSamples = pure $ singleSample
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


-- | Protocol parameters
data ProtocolParams = ProtocolParams
  { _protocolParamsEpoch                 :: Epoch -- ^ Epoch number
  , _protocolParamsMinFeeA               :: Integer -- ^ The linear factor for the minimum fee calculation for given epoch
  , _protocolParamsMinFeeB               :: Integer -- ^ The constant factor for the minimum fee calculation
  , _protocolParamsMaxBlockSize          :: Integer -- ^ Maximum block body size in Bytes
  , _protocolParamsMaxTxSize             :: Integer -- ^ Maximum transaction size
  , _protocolParamsMaxBlockHeaderSize    :: Integer -- ^ Maximum block header size
  , _protocolParamsKeyDeposit            :: Lovelaces -- ^ The amount of a key registration deposit in Lovelaces
  , _protocolParamsPoolDeposit           :: Lovelaces -- ^ The amount of a pool registration deposit in Lovelaces
  , _protocolParamsEMax                  :: Integer -- ^ Epoch bound on pool retirement
  , _protocolParamsNOpt                  :: Integer -- ^ Desired number of pools
  , _protocolParamsA0                    :: Double -- ^ Pool pledge influence
  , _protocolParamsRho                   :: Double -- ^ Monetary expansion
  , _protocolParamsTau                   :: Double -- ^ Treasury expansion
  , _protocolParamsDecentralisationParam :: Double -- ^ Percentage of blocks produced by federated nodes
-- ?? TODO: object Nullable
--  , protocolParamsExtraEntropy :: Maybe Value
  , _protocolParamsProtocolMajorVer      :: Integer -- ^ Accepted protocol major version
  , _protocolParamsProtocolMinorVer      :: Integer -- ^ Accepted protocol minor version
  , _protocolParamsMinUtxo               :: Lovelaces -- ^ Minimum UTXO value
  , _protocolParamsMinPoolCost           :: Lovelaces  -- ^ Minimum stake cost forced on the pool
  , _protocolParamsNonce                 :: Text -- ^ Epoch number only used once
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_protocolParams", CamelToSnake]] ProtocolParams

instance ToSample ProtocolParams where
  toSamples = pure $ singleSample
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
--      , _protocolParamsExtraEntropy = Nothing
      , _protocolParamsProtocolMajorVer = 2
      , _protocolParamsProtocolMinorVer = 0
      , _protocolParamsMinUtxo = 1000000
      , _protocolParamsMinPoolCost = 340000000
      , _protocolParamsNonce = "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81"
      }

-- | Active stake distribution for an epoch
data StakeDistribution = StakeDistribution
  { _stakeDistributionStakeAddress :: Address -- ^ Stake address
  , _stakeDistributionPoolId       :: PoolId -- ^ Bech32 prefix of the pool delegated to
  , _stakeDistributionAmount       :: Lovelaces -- ^ Amount of active delegated stake in Lovelaces
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_stakeDistribution", CamelToSnake]] StakeDistribution

instance ToSample StakeDistribution where
  toSamples = pure $ singleSample
    StakeDistribution
      { _stakeDistributionStakeAddress = "stake1u9l5q5jwgelgagzyt6nuaasefgmn8pd25c8e9qpeprq0tdcp0e3uk"
      , _stakeDistributionPoolId = "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      , _stakeDistributionAmount = 4440295078
      }

-- | Stake distribution for an epoch for specific pool
data PoolStakeDistribution = PoolStakeDistribution
  { _poolStakeDistributionStakeAddress :: Address -- ^ Stake address
  , _poolStakeDistributionAmount       :: Lovelaces -- ^ Amount of active delegated stake in Lovelaces
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_poolStakeDistribution", CamelToSnake]] PoolStakeDistribution

instance ToSample PoolStakeDistribution where
  toSamples = pure $ singleSample
    PoolStakeDistribution
      { _poolStakeDistributionStakeAddress = "stake1u9l5q5jwgelgagzyt6nuaasefgmn8pd25c8e9qpeprq0tdcp0e3uk"
      , _poolStakeDistributionAmount = 4440295078
      }
