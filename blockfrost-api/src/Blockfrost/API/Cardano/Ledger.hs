-- | Cardano Ledger endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Ledger
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Genesis

data LedgerAPI route =
  LedgerAPI
    {
      _genesis
        :: route
        :- Summary "Blockchain genesis"
        :> Description "Return the information about blockchain genesis."
        :> Get '[JSON] Genesis
    } deriving (Generic)
