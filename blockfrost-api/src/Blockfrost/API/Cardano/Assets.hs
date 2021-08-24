-- | Assets API endpoints

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.Cardano.Assets
  where

import Servant.API
import Servant.API.Generic

import Blockfrost.Types.Cardano.Assets
import Blockfrost.Types.Shared
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting

data AssetsAPI route =
  AssetsAPI
    {
      _listAssets
        :: route
        :- Summary "Assets"
        :> Description "List of assets."
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AssetInfo]
    , _assetDetails
        :: route
        :- Summary "Specific asset"
        :> Description "Information about a specific asset."
        :> Capture "asset" AssetId
        :> Get '[JSON] AssetDetails
    , _assetHistory
        :: route
        :- Summary "Asset history"
        :> Description "History of a specific asset."
        :> Capture "asset" AssetId
        :> "history"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AssetHistory]
    , _assetTransactions
        :: route
        :- Summary "Asset transactions"
        :> Description "List of a specific asset transactions"
        :> Capture "asset" AssetId
        :> "transactions"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AssetTransaction]
    , _assetAddresses
        :: route
        :- Summary "Asset addresses"
        :> Description "List of a addresses containing a specific asset"
        :> Capture "asset" AssetId
        :> "addresses"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AssetAddress]
    , _listAssetsPolicy
        :: route
        :- Summary "Assets of a specific policy"
        :> Description "List of asset minted under a specific policy."
        :> "policy"
        :> Capture "policy_id" PolicyId
        :> Pagination
        :> Sorting
        :> Get '[JSON] [AssetInfo]
    } deriving (Generic)
