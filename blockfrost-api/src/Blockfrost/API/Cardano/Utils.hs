-- | Cardano utility endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Utils
  where

import Data.Text
import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Utils

data UtilsAPI route =
  UtilsAPI
    {
      _deriveAddr
        :: route
        :- Summary "Derive an address"
        :> Description "Derive Shelley address from an xpub."
        :> "addresses"
        :> "xpub"
        :> Capture "xpub" Text
        :> Capture "role" Integer
        :> Capture "Index" Integer
        :> Get '[JSON] DerivedAddress
    } deriving (Generic)
