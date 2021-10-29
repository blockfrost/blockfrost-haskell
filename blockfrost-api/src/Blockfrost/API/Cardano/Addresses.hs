-- | Addresses API endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Addresses
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Addresses
import Blockfrost.Types.Shared
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data AddressesAPI route =
  AddressesAPI
    {
      _addressInfo
        :: route
        :- Summary "Specific address"
        :> Description "Obtain information about a specific address."
        :> Capture "address" Address
        :> Get '[JSON] AddressInfo
    , _addressDetails
        :: route
        :- Summary "Address details"
        :> Description "Obtain details about an address."
        :> Capture "address" Address
        :> "total"
        :> Get '[JSON] AddressDetails
    , _addressUtxos
        :: route
        :- Summary "Address UTXOs"
        :> Description "UTXOs of the address."
        :> Capture "address" Address
        :> "utxos"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AddressUtxo]
    , _addressUtxosAsset
        :: route
        :- Summary "Address UTXOs of a given asset"
        :> Description "UTXOs of the address."
        :> Capture "address" Address
        :> "utxos"
        :> Capture "asset" AssetId
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AddressUtxo]
    , _addressTransactions
        :: route
        :- Summary "Address transactions"
        :> Description "Transactions on the address."
        :> Capture "address" Address
        :> "transactions"
        :> Pagination
        :> Sorting
        :> QueryParam "from" BlockIndex
        :> QueryParam "to" BlockIndex
        :> Get '[JSON] [AddressTransaction]
    } deriving (Generic)
