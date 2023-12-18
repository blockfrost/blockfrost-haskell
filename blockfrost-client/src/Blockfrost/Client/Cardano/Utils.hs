-- | Utility queries

module Blockfrost.Client.Cardano.Utils
  ( deriveShelleyAddress
  ) where

import Data.Text (Text)
import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

utilsClient :: MonadBlockfrost m => Project -> UtilsAPI (AsClientT m)
utilsClient = fromServant . _utils . cardanoClient

-- | Derive Shelley address from xpub key
deriveShelleyAddress_
  :: MonadBlockfrost m
  => Project
  -> Text -- ^ Hexadecimal xpub
  -> Integer -- ^ Account role
  -> Integer -- ^ Address index
  -> m DerivedAddress
deriveShelleyAddress_ = _deriveAddr . utilsClient

-- | Derive Shelley address from xpub key
deriveShelleyAddress
  :: MonadBlockfrost m
  => Text -- ^ Hexadecimal xpub
  -> Integer -- ^ Account role
  -> Integer -- ^ Address index
  -> m DerivedAddress
deriveShelleyAddress xpub role index = go (\p -> deriveShelleyAddress_ p xpub role index)

