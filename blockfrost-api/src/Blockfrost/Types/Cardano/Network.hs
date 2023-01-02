-- | Cardano Network reponses

{-# LANGUAGE NumericUnderscores #-}

module Blockfrost.Types.Cardano.Network
  ( Network (..)
  , NetworkStake (..)
  , NetworkSupply (..)
  , NetworkEraSummary (..)
  , NetworkEraBound (..)
  , NetworkEraParameters (..)
  ) where

import Data.Time (NominalDiffTime)
import Data.Word (Word64)
import Deriving.Aeson
import Servant.Docs (ToSample (..), samples, singleSample)

import Blockfrost.Types.Shared

-- | Lovelace supply data
data NetworkSupply = NetworkSupply
  { _supplyMax         :: Lovelaces -- ^ Maximum supply in Lovelaces
  , _supplyTotal       :: Lovelaces -- ^ Current total (max supply - reserves) supply in Lovelaces
  , _supplyCirculating :: Lovelaces -- ^ Current circulating (UTXOs + withdrawables) supply in Lovelaces
  , _supplyLocked      :: Lovelaces -- ^ Current supply locked by scripts in Lovelaces
  , _supplyTreasury    :: Lovelaces -- ^ Current supply locked in treasury
  , _supplyReserves    :: Lovelaces -- ^ Current supply locked in reserves
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_supply", CamelToSnake]] NetworkSupply

netSupplySample :: NetworkSupply
netSupplySample =
  NetworkSupply
    { _supplyMax = 45_000_000_000_000_000
    , _supplyTotal = 32_890_715_183_299_160
    , _supplyCirculating = 32_412_601_976_210_393
    , _supplyLocked = 125_006_953_355
    , _supplyTreasury = 98_635_632_000_000
    , _supplyReserves = 46_635_632_000_000
    }

instance ToSample NetworkSupply where
  toSamples = pure $ singleSample netSupplySample

-- | Live and active stake of the whole network
data NetworkStake = NetworkStake
  { _stakeLive   :: Lovelaces -- ^ Current live stake in Lovelaces
  , _stakeActive :: Lovelaces -- ^ Current active stake in Lovelaces
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_stake", CamelToSnake]] NetworkStake

netStakeSample :: NetworkStake
netStakeSample =
  NetworkStake
    { _stakeLive = 23_204_950_463_991_654
    , _stakeActive = 22_210_233_523_456_321
    }

instance ToSample NetworkStake where
  toSamples = pure $ singleSample netStakeSample

-- | Detailed network information
data Network = Network
  { _networkSupply :: NetworkSupply -- ^ Supply data
  , _networkStake  :: NetworkStake -- ^ Stake data
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_network", CamelToSnake]] Network

instance ToSample Network where
  toSamples = pure $ singleSample $
    Network netSupplySample netStakeSample

-- | Time bounds of an era.
data NetworkEraBound = NetworkEraBound
  { _boundEpoch :: Epoch, -- ^ The epoch number bounding a specific era.
    _boundSlot  :: Slot, -- ^ The slot number bounding a specific era.
    _boundTime  :: NominalDiffTime -- ^ The time, relative to network system start, bounding a specific era.
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_bound", CamelToSnake]] NetworkEraBound

netEraBoundSample0 :: NetworkEraBound
netEraBoundSample0 =
  NetworkEraBound
    { _boundEpoch = 4
    , _boundSlot = 86_400
    , _boundTime = 1_728_000
    }

netEraBoundSample1 :: NetworkEraBound
netEraBoundSample1 =
  NetworkEraBound
    { _boundEpoch = 5
    , _boundSlot = 518_400
    , _boundTime = 2_160_00
    }

instance ToSample NetworkEraBound where
  toSamples = pure $ samples [netEraBoundSample0, netEraBoundSample1]

-- | Parameters for a network era which can vary between hardforks.
data NetworkEraParameters = NetworkEraParameters
  { _parametersEpochLength :: EpochLength, -- ^ Number of slots in an epoch.
    _parametersSlotLength  :: NominalDiffTime, -- ^ How long a slot lasts.
    _parametersSafeZone    :: Word64 -- ^ Number of slots from the tip of the ledger in which a hardfork will not happen.
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_parameters", CamelToSnake]] NetworkEraParameters

netEraParamSample :: NetworkEraParameters
netEraParamSample =
  NetworkEraParameters
    { _parametersEpochLength = 432_000
    , _parametersSlotLength = 1
    , _parametersSafeZone = 129_600
    }

instance ToSample NetworkEraParameters where
  toSamples = pure $ singleSample netEraParamSample

-- | Summary of information about network eras.
data NetworkEraSummary = NetworkEraSummary
  { _networkEraStart      :: NetworkEraBound, -- ^ Start of a specific era.
    _networkEraEnd        :: NetworkEraBound, -- ^ End of a specific era.
    _networkEraParameters :: NetworkEraParameters -- ^ Active parameters for a specific era.
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_networkEra", CamelToSnake]] NetworkEraSummary

instance ToSample NetworkEraSummary where
  toSamples = pure . singleSample $
    NetworkEraSummary
      { _networkEraStart = netEraBoundSample0
      , _networkEraEnd = netEraBoundSample1
      , _networkEraParameters = netEraParamSample
      }
