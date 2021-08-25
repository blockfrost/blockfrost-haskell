-- | Asset queries

module Blockfrost.Client.Cardano.Assets
  ( getAssets
  , getAssets'
  , getAssetDetails
  , getAssetHistory
  , getAssetHistory'
  , getAssetTransactions
  , getAssetTransactions'
  , getAssetAddresses
  , getAssetAddresses'
  , getAssetsByPolicy
  , getAssetsByPolicy'
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types

assetsClient :: Project -> AssetsAPI (AsClientT BlockfrostClient)
assetsClient = fromServant . _assets . cardanoClient

getAssets_ :: Project -> Paged -> SortOrder -> BlockfrostClient [AssetInfo]
getAssets_ = _listAssets . assetsClient

-- | List all assets
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssets' :: Paged -> SortOrder -> BlockfrostClient [AssetInfo]
getAssets' pg s = go (\p -> getAssets_ p pg s)

-- | List all assets
getAssets :: BlockfrostClient [AssetInfo]
getAssets = getAssets' def def

getAssetDetails_ :: Project -> AssetId -> BlockfrostClient AssetDetails
getAssetDetails_ = _assetDetails . assetsClient

-- | Information about a specific asset
getAssetDetails :: AssetId -> BlockfrostClient AssetDetails
getAssetDetails a = go (`getAssetDetails_` a)

getAssetHistory_ :: Project -> AssetId -> Paged -> SortOrder -> BlockfrostClient [AssetHistory]
getAssetHistory_ = _assetHistory . assetsClient

-- | History of a specific asset
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssetHistory' :: AssetId -> Paged -> SortOrder -> BlockfrostClient [AssetHistory]
getAssetHistory' a pg s = go (\p -> getAssetHistory_ p a pg s)

-- | History of a specific asset
getAssetHistory :: AssetId -> BlockfrostClient [AssetHistory]
getAssetHistory a = getAssetHistory' a def def

getAssetTransactions_ :: Project -> AssetId -> Paged -> SortOrder -> BlockfrostClient [AssetTransaction]
getAssetTransactions_ = _assetTransactions . assetsClient

-- | List of a specific asset transactions
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssetTransactions' :: AssetId -> Paged -> SortOrder -> BlockfrostClient [AssetTransaction]
getAssetTransactions' a pg s = go (\p -> getAssetTransactions_ p a pg s)

-- | List of a specific asset transactions
getAssetTransactions :: AssetId -> BlockfrostClient [AssetTransaction]
getAssetTransactions a = getAssetTransactions' a def def

getAssetAddresses_ :: Project -> AssetId -> Paged -> SortOrder -> BlockfrostClient [AssetAddress]
getAssetAddresses_ = _assetAddresses . assetsClient

-- | List of a addresses containing a specific asset
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssetAddresses' :: AssetId -> Paged -> SortOrder -> BlockfrostClient [AssetAddress]
getAssetAddresses' a pg s = go (\p -> getAssetAddresses_ p a pg s)

-- | List of a addresses containing a specific asset
getAssetAddresses :: AssetId -> BlockfrostClient [AssetAddress]
getAssetAddresses a = getAssetAddresses' a def def

getAssetsByPolicy_ :: Project -> PolicyId -> Paged -> SortOrder -> BlockfrostClient [AssetInfo]
getAssetsByPolicy_ = _listAssetsPolicy . assetsClient

-- | List of asset minted under a specific policy
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssetsByPolicy' :: PolicyId -> Paged -> SortOrder -> BlockfrostClient [AssetInfo]
getAssetsByPolicy' a pg s = go (\p -> getAssetsByPolicy_ p a pg s)

-- | List of asset minted under a specific policy
getAssetsByPolicy :: PolicyId -> BlockfrostClient [AssetInfo]
getAssetsByPolicy a = getAssetsByPolicy' a def def
