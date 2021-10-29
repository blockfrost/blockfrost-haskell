-- | Blockfrost API definitions

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API
  ( BlockfrostAPI (..)
  , BlockfrostV0API (..)
  , CommonAPI (..)
  , CardanoAPI (..)
  , IPFSAPI (..)
  , NutLinkAPI (..)
  , module Blockfrost.API.Cardano
  , api
  , api0
  , ServantBlockfrostAPI
  , BlockfrostAuth
  , Project (..)
  , Form (..)
  ) where

import Data.Proxy (Proxy (..))
import Servant.API
import Servant.API.Generic

import Blockfrost.API.Cardano
import Blockfrost.API.Common
import Blockfrost.API.IPFS
import Blockfrost.API.NutLink
import Blockfrost.Auth
import Blockfrost.Types
import Blockfrost.Util.Tag (Tag)
import Blockfrost.Util.UserAgent (UserAgent)

-- * API

-- ** Our custom auth

type BlockfrostAuth = ProjectAuth '[APIKeyInHeader "project_id"] Project

-- ** Generic API types

-- *** Toplevel

newtype BlockfrostAPI route =
  BlockfrostAPI
    {
      _apiV0 :: route
      :- "api"
      :> "v0"
      :> BlockfrostAuth
      :> UserAgent
      :> ToServantApi BlockfrostV0API
    } deriving (Generic)

-- *** V0, requiring auth

data BlockfrostV0API route =
  BlockfrostV0API
    {
      _common  :: route :- ToServantApi CommonAPI
    , _cardano :: route :- ToServantApi CardanoAPI
    , _ipfs    :: route :- "ipfs" :> ToServantApi IPFSAPI
    , _nutLink :: route :- "nutlink"  :> Tag "Nut.link" :> ToServantApi NutLinkAPI
    } deriving (Generic)

-- *** Services

data CardanoAPI route =
  CardanoAPI
    {
      _accounts
      :: route
      :- "accounts"
      :> Tag "Cardano » Accounts"
      :> ToServantApi AccountsAPI
    , _addresses
      :: route
      :- "addresses"
      :> Tag "Cardano » Addresses"
      :> ToServantApi AddressesAPI
    , _assets
      :: route
      :- "assets"
      :> Tag "Cardano » Assets"
      :> ToServantApi AssetsAPI
    , _blocks
      :: route
      :- "blocks"
      :> Tag "Cardano » Blocks"
      :> ToServantApi BlocksAPI
    , _epochs
      :: route
      :- "epochs"
      :> Tag "Cardano » Epochs"
      :> ToServantApi EpochsAPI
    , _ledger
      :: route
      :- "genesis"
      :> Tag "Cardano » Ledger"
      :> ToServantApi LedgerAPI
    , _metadata
      :: route
      :- "metadata"
      :> Tag "Cardano » Metadata"
      :> ToServantApi MetadataAPI
    , _network
      :: route
      :- "network"
      :> Tag "Cardano » Network"
      :> ToServantApi NetworkAPI
    , _pools
      :: route
      :- "pools"
      :> Tag "Cardano » Pools"
      :> ToServantApi PoolsAPI
    , _scripts
      :: route
      :- "scripts"
      :> Tag "Cardano » Scripts"
      :> ToServantApi ScriptsAPI
    , _transactions
      :: route
      :- "txs"
      :> Tag "Cardano » Transactions"
      :> ToServantApi TransactionsAPI
    , _txSubmit
      :: route
      :- Summary "Submit a transaction"
      :> Description "Submit an already serialized transaction to the network."
      :> Tag "Cardano » Transactions"
      :> "tx"
      :> "submit"
      :> ReqBody '[CBOR] CBORString
      :> Post '[JSON] TxHash
    } deriving (Generic)

type ServantBlockfrostAPI = ToServantApi BlockfrostAPI

api :: Proxy (ToServantApi BlockfrostAPI)
api = genericApi (Proxy :: Proxy BlockfrostAPI)

api0 :: Proxy (ToServantApi BlockfrostV0API)
api0 = genericApi (Proxy :: Proxy BlockfrostV0API)
