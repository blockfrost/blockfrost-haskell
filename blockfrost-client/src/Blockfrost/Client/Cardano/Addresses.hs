-- | Address queries

module Blockfrost.Client.Cardano.Addresses
  ( getAddressInfo
  , getAddressDetails
  , getAddressUtxos
  , getAddressUtxos'
  , getAddressTransactions
  , getAddressTransactions'
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

addressesClient :: Project -> AddressesAPI (AsClientT BlockfrostClient)
addressesClient = fromServant . _addresses . cardanoClient

getAddressInfo_ :: Project -> Address -> BlockfrostClient AddressInfo
getAddressInfo_ = _addressInfo . addressesClient

-- | Obtain information about a specific address.
getAddressInfo :: Address -> BlockfrostClient AddressInfo
getAddressInfo a = go (`getAddressInfo_` a)

getAddressDetails_ :: Project -> Address -> BlockfrostClient AddressDetails
getAddressDetails_ = _addressDetails . addressesClient

-- | Obtain details about an address.
getAddressDetails :: Address -> BlockfrostClient AddressDetails
getAddressDetails a = go (`getAddressDetails_` a)

getAddressUtxos_ :: Project -> Address -> Paged -> SortOrder -> BlockfrostClient [AddressUtxo]
getAddressUtxos_ = _addressUtxos . addressesClient

-- | UTXOs of the address.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAddressUtxos' :: Address -> Paged -> SortOrder -> BlockfrostClient [AddressUtxo]
getAddressUtxos' a pg s = go (\p -> getAddressUtxos_ p a pg s)

-- | UTXOs of the address.
getAddressUtxos :: Address -> BlockfrostClient [AddressUtxo]
getAddressUtxos a = getAddressUtxos' a def def

getAddressTransactions_ ::
--- Project -> Address -> BlockfrostClient [AddressTransaction]
     Project
  -> Address
  -> Paged
  -> SortOrder
  -> Maybe BlockIndex
  -> Maybe BlockIndex
  -> BlockfrostClient [AddressTransaction]
getAddressTransactions_ = _addressTransactions . addressesClient

-- | Transactions on the address.
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
-- Also allows support for limiting block ranges using `from`/`to`
-- @BlockIndex@es.
getAddressTransactions' ::
     Address
  -> Paged
  -> SortOrder
  -> Maybe BlockIndex
  -> Maybe BlockIndex
  -> BlockfrostClient [AddressTransaction]
getAddressTransactions' a pg s from to = go (\p -> getAddressTransactions_ p a pg s from to)

-- | Transactions on the address.
getAddressTransactions :: Address -> BlockfrostClient [AddressTransaction]
getAddressTransactions a = getAddressTransactions' a def def Nothing Nothing
