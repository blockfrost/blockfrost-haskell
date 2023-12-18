-- | Utility queries

module Blockfrost.Client.Cardano.Utils
  ( deriveShelleyAddress
  , txEvaluate
  , txEvaluateUTXOs
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

txEvaluate_
  :: MonadBlockfrost m
  => Project
  -> CBORString
  -> m TxEval
txEvaluate_ = _txEvaluate . utilsClient

-- | Submit a transaction for execution units evaluation
txEvaluate
  :: MonadBlockfrost m
  => CBORString
  -> m TxEval
txEvaluate txCbor = go (`txEvaluate_` txCbor)

txEvaluateUTXOs_
  :: MonadBlockfrost m
  => Project
  -> TxEvalInput
  -> m TxEval
txEvaluateUTXOs_ = _txEvaluateUTXOs . utilsClient

-- | Submit a transaction for execution units evaluation (additional UTXO set)
txEvaluateUTXOs
  :: MonadBlockfrost m
  => TxEvalInput
  -> m TxEval
txEvaluateUTXOs txei = go (`txEvaluateUTXOs_` txei)
