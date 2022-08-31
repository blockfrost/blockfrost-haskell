-- | Blockchain genesis

{-# LANGUAGE NumericUnderscores #-}

module Blockfrost.Types.Cardano.Genesis
  ( Genesis (..)
  ) where

import Deriving.Aeson
import Servant.Docs (ToSample (..), singleSample)

import Blockfrost.Types.Shared

-- | Information about blockchain genesis
data Genesis = Genesis
  { _genesisActiveSlotsCoefficient :: Rational -- ^ The proportion of slots in which blocks should be issued
  , _genesisUpdateQuorum           :: Integer -- ^ Determines the quorum needed for votes on the protocol parameter updates
  , _genesisMaxLovelaceSupply      :: Lovelaces -- ^ The total number of lovelace in the system
  , _genesisNetworkMagic           :: Integer -- ^ Network identifier
  , _genesisEpochLength            :: Integer -- ^ Number of slots in an epoch
  , _genesisSystemStart            :: POSIXTime -- ^ Time of slot 0 in UNIX time
  , _genesisSlotsPerKesPeriod      :: Integer -- ^ Number of slots in an KES period
  , _genesisSlotLength             :: Integer -- ^ Duration of one slot in seconds
  , _genesisMaxKesEvolutions       :: Integer -- ^ The maximum number of time a KES key can be evolved before a pool operator must create a new operational certificate
  , _genesisSecurityParam          :: Integer -- ^ Security parameter @k@
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_genesis", CamelToSnake]] Genesis

instance ToSample Genesis where
  toSamples _ = singleSample $
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
