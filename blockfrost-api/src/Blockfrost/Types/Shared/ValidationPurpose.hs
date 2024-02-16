-- | Transaction or script redeemer validation purpose
module Blockfrost.Types.Shared.ValidationPurpose
  ( ValidationPurpose (..)
  ) where

import Deriving.Aeson
import Servant.Docs (ToSample (..), samples)

import Blockfrost.Types.Shared.Opts

-- | Validation purpose
data ValidationPurpose = Spend | Mint | Cert | Reward
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ConstructorTagModifier '[ToLower]] ValidationPurpose

instance ToSample ValidationPurpose where
  toSamples = pure $ samples [ Spend, Mint, Cert, Reward ]


