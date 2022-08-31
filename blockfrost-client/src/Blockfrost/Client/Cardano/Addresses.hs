-- | Address queries

module Blockfrost.Client.Cardano.Addresses
  ( getAddressInfo
  , getAddressDetails
  , getAddressUtxos
  , getAddressUtxos'
  , getAddressUtxosAsset
  , getAddressUtxosAsset'
  , getAddressTransactions
  , getAddressTransactions'
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

addressesClient :: MonadBlockfrost m => Project -> AddressesAPI (AsClientT m)
addressesClient = fromServant . _addresses . cardanoClient

getAddressInfo_ :: MonadBlockfrost m => Project -> Address -> m AddressInfo
getAddressInfo_ = _addressInfo . addressesClient

-- | Obtain information about a specific address.
getAddressInfo :: MonadBlockfrost m => Address -> m AddressInfo
getAddressInfo a = go (`getAddressInfo_` a)

getAddressDetails_ :: MonadBlockfrost m => Project -> Address -> m AddressDetails
getAddressDetails_ = _addressDetails . addressesClient

-- | Obtain details about an address.
getAddressDetails :: MonadBlockfrost m => Address -> m AddressDetails
getAddressDetails a = go (`getAddressDetails_` a)

getAddressUtxos_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [AddressUtxo]
getAddressUtxos_ = _addressUtxos . addressesClient

-- | UTXOs of the address.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAddressUtxos' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [AddressUtxo]
getAddressUtxos' a pg s = go (\p -> getAddressUtxos_ p a pg s)

-- | UTXOs of the address.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAddressUtxos :: MonadBlockfrost m => Address -> m [AddressUtxo]
getAddressUtxos a = getAddressUtxos' a def def

getAddressUtxosAsset_ :: MonadBlockfrost m => Project -> Address -> AssetId -> Paged -> SortOrder -> m [AddressUtxo]
getAddressUtxosAsset_ = _addressUtxosAsset . addressesClient

-- | UTXOs of the address containing specific asset.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
getAddressUtxosAsset' :: MonadBlockfrost m => Address -> AssetId-> Paged -> SortOrder -> m [AddressUtxo]
getAddressUtxosAsset' addr asset pg s = go (\p -> getAddressUtxosAsset_ p addr asset pg s)

-- | UTXOs of the address containing specific asset.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAddressUtxosAsset :: MonadBlockfrost m => Address -> AssetId -> m [AddressUtxo]
getAddressUtxosAsset addr asset = getAddressUtxosAsset' addr asset def def

getAddressTransactions_ ::
     MonadBlockfrost m
  => Project
  -> Address
  -> Paged
  -> SortOrder
  -> Maybe BlockIndex
  -> Maybe BlockIndex
  -> m [AddressTransaction]
getAddressTransactions_ = _addressTransactions . addressesClient

-- | Transactions on the address.
-- Allows custom paging and ordering using 'Paged' and 'SortOrder'.
-- Also allows support for limiting block ranges using `from`/`to`
-- @BlockIndex@es.
getAddressTransactions' ::
     MonadBlockfrost m
  => Address
  -> Paged
  -> SortOrder
  -> Maybe BlockIndex
  -> Maybe BlockIndex
  -> m [AddressTransaction]
getAddressTransactions' a pg s from to = go (\p -> getAddressTransactions_ p a pg s from to)

-- | Transactions on the address.
--
-- Queries 100 entries. To query all entries use 'Blockfrost.Client.Core.allPages'
-- with principled variant of this function (suffixed with @'@)
-- that accepts 'Paged' argument.
getAddressTransactions :: MonadBlockfrost m => Address -> m [AddressTransaction]
getAddressTransactions a = getAddressTransactions' a def def Nothing Nothing
