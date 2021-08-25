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

nutlinkListAddress_ :: Project -> Address-> BlockfrostClient NutlinkAddress
nutlinkListAddress_ = _address . nutLinkClient

-- | List metadata about specific address
nutlinkListAddress :: Address -> BlockfrostClient NutlinkAddress
nutlinkListAddress a = go (`nutlinkListAddress_` a)

nutlinkListAddressTickers_ :: Project -> Address -> Paged -> SortOrder -> BlockfrostClient [NutlinkAddressTicker]
nutlinkListAddressTickers_ = _listAddressTickers . nutLinkClient

-- | List tickers for a specific metadata oracle
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
nutlinkListAddressTickers' :: Address -> Paged -> SortOrder -> BlockfrostClient [NutlinkAddressTicker]
nutlinkListAddressTickers' a pg s = go (\p -> nutlinkListAddressTickers_ p a pg s)

-- | List tickers for a specific metadata oracle
nutlinkListAddressTickers :: Address -> BlockfrostClient [NutlinkAddressTicker]
nutlinkListAddressTickers a = nutlinkListAddressTickers' a def def

nutlinkAddressTickers_ :: Project -> Address -> Text -> Paged -> SortOrder -> BlockfrostClient [NutlinkTicker]
nutlinkAddressTickers_ = _addressTickers . nutLinkClient

-- | List of records of a specific ticker
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
nutlinkAddressTickers' :: Address -> Text -> Paged -> SortOrder -> BlockfrostClient [NutlinkTicker]
nutlinkAddressTickers' a t pg s = go (\p -> nutlinkAddressTickers_ p a t pg s)

-- | List of records of a specific ticker
nutlinkAddressTickers :: Address -> Text -> BlockfrostClient [NutlinkTicker]
nutlinkAddressTickers a t = nutlinkAddressTickers' a t def def

nutlinkTickers_ :: Project -> Text -> Paged -> SortOrder -> BlockfrostClient [(Address, NutlinkTicker)]
nutlinkTickers_ = _tickers . nutLinkClient

-- | List of records of a specific ticker
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
nutlinkTickers' :: Text -> Paged -> SortOrder -> BlockfrostClient [(Address, NutlinkTicker)]
nutlinkTickers' a pg s = go (\p -> nutlinkTickers_ p a pg s)

-- | List of records of a specific ticker
nutlinkTickers :: Text -> BlockfrostClient [(Address, NutlinkTicker)]
nutlinkTickers a = nutlinkTickers' a def def
