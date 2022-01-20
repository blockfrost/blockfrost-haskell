-- | Ledger queries

module Blockfrost.Client.Cardano.Ledger
  ( getLedgerGenesis
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

ledgerClient :: MonadBlockfrost m => Project -> LedgerAPI (AsClientT m)
ledgerClient = fromServant . _ledger . cardanoClient

getLedgerGenesis_ :: MonadBlockfrost m => Project -> m Genesis
getLedgerGenesis_ = _genesis . ledgerClient

-- | Get the information about blockchain genesis.
getLedgerGenesis :: MonadBlockfrost m => m Genesis
getLedgerGenesis = go getLedgerGenesis_
