-- | Nut.link client functions

module Blockfrost.Client.NutLink
  ( nutlinkAddressTickers
  , nutlinkAddressTickers'
  , nutlinkListAddress
  , nutlinkListAddressTickers
  , nutlinkListAddressTickers'
  , nutlinkTickers
  , nutlinkTickers'
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types
import Data.Text (Text)

nutlinkListAddress_ :: MonadBlockfrost m => Project -> Address-> m NutlinkAddress
nutlinkListAddress_ = _address . nutLinkClient

-- | List metadata about specific address
nutlinkListAddress :: MonadBlockfrost m => Address -> m NutlinkAddress
nutlinkListAddress a = go (`nutlinkListAddress_` a)

nutlinkListAddressTickers_ :: MonadBlockfrost m => Project -> Address -> Paged -> SortOrder -> m [NutlinkAddressTicker]
nutlinkListAddressTickers_ = _listAddressTickers . nutLinkClient

-- | List tickers for a specific metadata oracle
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
nutlinkListAddressTickers' :: MonadBlockfrost m => Address -> Paged -> SortOrder -> m [NutlinkAddressTicker]
nutlinkListAddressTickers' a pg s = go (\p -> nutlinkListAddressTickers_ p a pg s)

-- | List tickers for a specific metadata oracle
nutlinkListAddressTickers :: MonadBlockfrost m => Address -> m [NutlinkAddressTicker]
nutlinkListAddressTickers a = nutlinkListAddressTickers' a def def

nutlinkAddressTickers_ :: MonadBlockfrost m => Project -> Address -> Text -> Paged -> SortOrder -> m [NutlinkTicker]
nutlinkAddressTickers_ = _addressTickers . nutLinkClient

-- | List of records of a specific ticker
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
nutlinkAddressTickers' :: MonadBlockfrost m => Address -> Text -> Paged -> SortOrder -> m [NutlinkTicker]
nutlinkAddressTickers' a t pg s = go (\p -> nutlinkAddressTickers_ p a t pg s)

-- | List of records of a specific ticker
nutlinkAddressTickers :: MonadBlockfrost m => Address -> Text -> m [NutlinkTicker]
nutlinkAddressTickers a t = nutlinkAddressTickers' a t def def

nutlinkTickers_ :: MonadBlockfrost m => Project -> Text -> Paged -> SortOrder -> m [(Address, NutlinkTicker)]
nutlinkTickers_ = _tickers . nutLinkClient

-- | List of records of a specific ticker
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
nutlinkTickers' :: MonadBlockfrost m => Text -> Paged -> SortOrder -> m [(Address, NutlinkTicker)]
nutlinkTickers' a pg s = go (\p -> nutlinkTickers_ p a pg s)

-- | List of records of a specific ticker
nutlinkTickers :: MonadBlockfrost m => Text -> m [(Address, NutlinkTicker)]
nutlinkTickers a = nutlinkTickers' a def def
