-- | Ledger queries

module Blockfrost.Client.Cardano.Ledger
  ( getLedgerGenesis
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

ledgerClient :: Project -> LedgerAPI (AsClientT BlockfrostClient)
ledgerClient = fromServant . _ledger . cardanoClient

getLedgerGenesis_ :: Project -> BlockfrostClient Genesis
getLedgerGenesis_ = _genesis . ledgerClient

-- | Get the information about blockchain genesis.
getLedgerGenesis:: BlockfrostClient Genesis
getLedgerGenesis = go getLedgerGenesis_
