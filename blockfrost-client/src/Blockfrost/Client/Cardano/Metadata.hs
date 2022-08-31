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

metadataClient :: MonadBlockfrost m => Project -> MetadataAPI (AsClientT m)
metadataClient = fromServant . _metadata . cardanoClient

getTxMetadataLabels_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [TxMeta]
getTxMetadataLabels_ = _txMetadataLabels . metadataClient

-- | List of all used transaction metadata labels.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getTxMetadataLabels' :: MonadBlockfrost m => Paged -> SortOrder -> m [TxMeta]
getTxMetadataLabels' pg s = go (\p -> getTxMetadataLabels_ p pg s)

-- | List of all used transaction metadata labels.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getTxMetadataLabels :: MonadBlockfrost m => m [TxMeta]
getTxMetadataLabels = getTxMetadataLabels' def def

getTxMetadataByLabelJSON_ :: MonadBlockfrost m => Project -> Text -> Paged -> SortOrder -> m [TxMetaJSON]
getTxMetadataByLabelJSON_ = _txMetadataByLabelJSON . metadataClient

-- | Transaction metadata per label (JSON @Value@)
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getTxMetadataByLabelJSON' :: MonadBlockfrost m => Text -> Paged -> SortOrder -> m [TxMetaJSON]
getTxMetadataByLabelJSON' t pg s = go (\p -> getTxMetadataByLabelJSON_ p t pg s)

-- | Transaction metadata per label (JSON @Value@)
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getTxMetadataByLabelJSON :: MonadBlockfrost m => Text -> m [TxMetaJSON]
getTxMetadataByLabelJSON t = getTxMetadataByLabelJSON' t def def

getTxMetadataByLabelCBOR_ :: MonadBlockfrost m => Project -> Text -> Paged -> SortOrder -> m [TxMetaCBOR]
getTxMetadataByLabelCBOR_ = _txMetadataByLabelCBOR . metadataClient

-- | Transaction metadata per label (CBOR @ByteString@)
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getTxMetadataByLabelCBOR' :: MonadBlockfrost m => Text -> Paged -> SortOrder -> m [TxMetaCBOR]
getTxMetadataByLabelCBOR' t pg s = go (\p -> getTxMetadataByLabelCBOR_ p t pg s)

-- | Transaction metadata per label (CBOR @ByteString@)
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getTxMetadataByLabelCBOR :: MonadBlockfrost m => Text -> m [TxMetaCBOR]
getTxMetadataByLabelCBOR t = getTxMetadataByLabelCBOR' t def def
