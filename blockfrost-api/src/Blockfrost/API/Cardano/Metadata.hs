-- | Cardano Metadata endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Metadata
  where

import Data.Text (Text)
import Servant.API
import Servant.API.Generic
import Servant.Docs (DocCapture (..), ToCapture (..))

import Blockfrost.Types.Cardano.Metadata
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data MetadataAPI route =
  MetadataAPI
    {
      _txMetadataLabels
        :: route
        :- Summary "Transaction metadata labels"
        :> Description "List of all used transaction metadata labels."
        :> "txs"
        :> "labels"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [TxMeta]
    , _txMetadataByLabelJSON
        :: route
        :- Summary "Transaction metadata content in JSON"
        :> Description "Transaction metadata per label."
        :> "txs"
        :> "labels"
        :> Capture "label" Text
        :> Pagination
        :> Sorting
        :> Get '[JSON] [TxMetaJSON]
    , _txMetadataByLabelCBOR
        :: route
        :- Summary "Transaction metadata content in CBOR"
        :> Description "Transaction metadata per label."
        :> "txs"
        :> "labels"
        :> Capture "label" Text
        :> "cbor"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [TxMetaCBOR]
    } deriving (Generic)

instance ToCapture (Capture "label" Text) where
  toCapture _ = DocCapture "label" "Metadata label"
