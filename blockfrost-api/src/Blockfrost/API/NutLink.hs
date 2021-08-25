-- | Nut.link services

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.NutLink
  where

import Blockfrost.Types
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting
import Data.Text (Text)
import Servant.API
import Servant.API.Generic
import Servant.Docs (DocCapture (..), ToCapture (..))

data NutLinkAPI route =
  NutLinkAPI
    {
      _address
        :: route
        :- Summary "List metadata about specific address"
        :> Description "List metadata about specific address"
        :> Capture "address" Address
        :> Get '[JSON] NutlinkAddress
    , _listAddressTickers
        :: route
        :- Summary "List tickers for a specific metadata oracle"
        :> Description "List tickers for a specific metadata oracle"
        :> Capture "address" Address
        :> "tickers"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [NutlinkAddressTicker]
    , _addressTickers
        :: route
        :- Summary "List of records of a specific ticker"
        :> Description "List of records of a specific ticker"
        :> Capture "address" Address
        :> "tickers"
        :> Capture "ticker" Text
        :> Pagination
        :> Sorting
        :> Get '[JSON] [NutlinkTicker]
    , _tickers
        :: route
        :- Summary "List of records of a specific ticker"
        :> Description "List of records of a specific ticker"
        :> "tickers"
        :> Capture "ticker" Text
        :> Pagination
        :> Sorting
        :> Get '[JSON] [(Address, NutlinkTicker)]
    } deriving (Generic)

instance ToCapture (Capture "ticker" Text) where
  toCapture _ = DocCapture "ticker" "Ticker for the pool record"
