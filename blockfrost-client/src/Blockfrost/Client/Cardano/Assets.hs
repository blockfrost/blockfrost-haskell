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

assetsClient :: MonadBlockfrost m => Project -> AssetsAPI (AsClientT m)
assetsClient = fromServant . _assets . cardanoClient

getAssets_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [AssetInfo]
getAssets_ = _listAssets . assetsClient

-- | List all assets
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssets' :: MonadBlockfrost m => Paged -> SortOrder -> m [AssetInfo]
getAssets' pg s = go (\p -> getAssets_ p pg s)

-- | List all assets
getAssets :: MonadBlockfrost m => m [AssetInfo]
getAssets = getAssets' def def

getAssetDetails_ :: MonadBlockfrost m => Project -> AssetId -> m AssetDetails
getAssetDetails_ = _assetDetails . assetsClient

-- | Information about a specific asset
getAssetDetails :: MonadBlockfrost m => AssetId -> m AssetDetails
getAssetDetails a = go (`getAssetDetails_` a)

getAssetHistory_ :: MonadBlockfrost m => Project -> AssetId -> Paged -> SortOrder -> m [AssetHistory]
getAssetHistory_ = _assetHistory . assetsClient

-- | History of a specific asset
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssetHistory' :: MonadBlockfrost m => AssetId -> Paged -> SortOrder -> m [AssetHistory]
getAssetHistory' a pg s = go (\p -> getAssetHistory_ p a pg s)

-- | History of a specific asset
getAssetHistory :: MonadBlockfrost m => AssetId -> m [AssetHistory]
getAssetHistory a = getAssetHistory' a def def

getAssetTransactions_ :: MonadBlockfrost m => Project -> AssetId -> Paged -> SortOrder -> m [AssetTransaction]
getAssetTransactions_ = _assetTransactions . assetsClient

-- | List of a specific asset transactions
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssetTransactions' :: MonadBlockfrost m => AssetId -> Paged -> SortOrder -> m [AssetTransaction]
getAssetTransactions' a pg s = go (\p -> getAssetTransactions_ p a pg s)

-- | List of a specific asset transactions
getAssetTransactions :: MonadBlockfrost m => AssetId -> m [AssetTransaction]
getAssetTransactions a = getAssetTransactions' a def def

getAssetAddresses_ :: MonadBlockfrost m => Project -> AssetId -> Paged -> SortOrder -> m [AssetAddress]
getAssetAddresses_ = _assetAddresses . assetsClient

-- | List of a addresses containing a specific asset
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssetAddresses' :: MonadBlockfrost m => AssetId -> Paged -> SortOrder -> m [AssetAddress]
getAssetAddresses' a pg s = go (\p -> getAssetAddresses_ p a pg s)

-- | List of a addresses containing a specific asset
getAssetAddresses :: MonadBlockfrost m => AssetId -> m [AssetAddress]
getAssetAddresses a = getAssetAddresses' a def def

getAssetsByPolicy_ :: MonadBlockfrost m => Project -> PolicyId -> Paged -> SortOrder -> m [AssetInfo]
getAssetsByPolicy_ = _listAssetsPolicy . assetsClient

-- | List of asset minted under a specific policy
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
getAssetsByPolicy' :: MonadBlockfrost m => PolicyId -> Paged -> SortOrder -> m [AssetInfo]
getAssetsByPolicy' a pg s = go (\p -> getAssetsByPolicy_ p a pg s)

-- | List of asset minted under a specific policy
getAssetsByPolicy :: MonadBlockfrost m => PolicyId -> m [AssetInfo]
getAssetsByPolicy a = getAssetsByPolicy' a def def
