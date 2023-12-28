-- | Cardano utility endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Utils
  where

import Data.Text
import Servant.API
import Servant.API.Generic
import Servant.Docs (DocCapture (..), ToCapture (..))

import Blockfrost.Types.Cardano.Utils
import Blockfrost.Types.Shared.CBOR

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
        :> Capture "index" Integer
        :> Get '[JSON] DerivedAddress

    , _txEvaluate
        :: route
        :- Summary "Submit a transaction for execution units evaluation"
        :> Description "Submit an already serialized transaction to evaluate \
                       \how much execution units it requires."
        :> "txs"
        :> "evaluate"
        :> ReqBody '[CBOR] CBORString
        :> Post '[JSON] TxEval

    , _txEvaluateUTXOs
        :: route
        :- Summary "Submit a transaction for execution units evaluation (additional UTXO set)"
        :> Description "Submit a JSON payload with transaction CBOR and additional UTXO set \
                       \to evaluate how much execution units it requires."
        :> "txs"
        :> "evaluate"
        :> "utxos"
        :> ReqBody '[JSON] TxEvalInput
        :> Post '[JSON] TxEval

    } deriving (Generic)

instance ToCapture (Capture "role" Integer) where
  toCapture _ = DocCapture "role" "Account role"

instance ToCapture (Capture "index" Integer) where
  toCapture _ = DocCapture "index" "Address index"

instance ToCapture (Capture "xpub" Text) where
  toCapture _ = DocCapture "xpub" "Hexadecimal xpub"
