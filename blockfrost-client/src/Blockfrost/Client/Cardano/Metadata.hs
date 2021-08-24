-- | Metadata queries

module Blockfrost.Client.Cardano.Metadata
  ( getTxMetadataLabels
  , getTxMetadataLabels'
  , getTxMetadataByLabelJSON
  , getTxMetadataByLabelJSON'
  , getTxMetadataByLabelCBOR
  , getTxMetadataByLabelCBOR'
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types
import Data.Text (Text)

metadataClient :: Project -> MetadataAPI (AsClientT BlockfrostClient)
metadataClient = fromServant . _metadata . cardanoClient

getTxMetadataLabels_ :: Project -> Paged -> SortOrder -> BlockfrostClient [TxMeta]
getTxMetadataLabels_ = _txMetadataLabels . metadataClient

-- | List of all used transaction metadata labels.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getTxMetadataLabels' :: Paged -> SortOrder -> BlockfrostClient [TxMeta]
getTxMetadataLabels' pg s = go (\p -> getTxMetadataLabels_ p pg s)

-- | List of all used transaction metadata labels.
getTxMetadataLabels :: BlockfrostClient [TxMeta]
getTxMetadataLabels = getTxMetadataLabels' def def

getTxMetadataByLabelJSON_ :: Project -> Text -> Paged -> SortOrder -> BlockfrostClient [TxMetaJSON]
getTxMetadataByLabelJSON_ = _txMetadataByLabelJSON . metadataClient

-- | Transaction metadata per label (JSON @Value@)
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getTxMetadataByLabelJSON' :: Text -> Paged -> SortOrder -> BlockfrostClient [TxMetaJSON]
getTxMetadataByLabelJSON' t pg s = go (\p -> getTxMetadataByLabelJSON_ p t pg s)

-- | Transaction metadata per label (JSON @Value@)
getTxMetadataByLabelJSON :: Text -> BlockfrostClient [TxMetaJSON]
getTxMetadataByLabelJSON t = getTxMetadataByLabelJSON' t def def

getTxMetadataByLabelCBOR_ :: Project -> Text -> Paged -> SortOrder -> BlockfrostClient [TxMetaCBOR]
getTxMetadataByLabelCBOR_ = _txMetadataByLabelCBOR . metadataClient

-- | Transaction metadata per label (CBOR @ByteString@)
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getTxMetadataByLabelCBOR' :: Text -> Paged -> SortOrder -> BlockfrostClient [TxMetaCBOR]
getTxMetadataByLabelCBOR' t pg s = go (\p -> getTxMetadataByLabelCBOR_ p t pg s)

-- | Transaction metadata per label (CBOR @ByteString@)
getTxMetadataByLabelCBOR :: Text -> BlockfrostClient [TxMetaCBOR]
getTxMetadataByLabelCBOR t = getTxMetadataByLabelCBOR' t def def
