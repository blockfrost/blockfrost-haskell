-- | Cardano Network reponses

{-# LANGUAGE NumericUnderscores #-}

module Blockfrost.Types.Cardano.Network
  ( Network (..)
  , NetworkStake (..)
  , NetworkSupply (..)
  ) where

import Deriving.Aeson
import Servant.Docs (ToSample (..), singleSample)

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
