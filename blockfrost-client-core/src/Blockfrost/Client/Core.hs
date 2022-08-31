-- | Core shared by clients
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockfrost.Client.Core
  ( BlockfrostError (..)
  , Paged (..)
  , SortOrder (..)
  , asc
  , def
  , desc
  , fromServantClientError
  , newEnvByProject
  , page
  , paged
  , allPages
  , nextPage
  , projectFromEnv
  , projectFromEnv'
  , projectFromFile
  ) where

import Blockfrost.API (Form (..))
import Blockfrost.Auth
import Blockfrost.Client.Auth ()
import Blockfrost.Client.Pagination
import Blockfrost.Client.Sorting
import Blockfrost.Client.Tag ()
import Blockfrost.Client.UserAgent ()
import Blockfrost.Types.ApiError
import Data.Aeson (eitherDecode)
import Data.Default (Default (def))
import Data.Text (Text)
import qualified Data.Text.IO
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.TLS
import Network.HTTP.Types
import Servant.Client
import Servant.Multipart.API
import Servant.Multipart.Client ()
import qualified System.Environment

domain :: String
domain = "blockfrost.io"

newEnvByProject :: Project -> IO ClientEnv
newEnvByProject prj = do
  conman <- Network.HTTP.Client.newManager Network.HTTP.Client.TLS.tlsManagerSettings
  pure $ mkClientEnv conman (baseUrlByEnv $ projectEnv prj)

buildUrl :: String -> BaseUrl
buildUrl subdomain =
  BaseUrl
    Https
    (subdomain <> "." <> domain)
    443
    mempty

baseUrlByEnv :: Env -> BaseUrl
baseUrlByEnv Localhost = BaseUrl Http "localhost" 8000 ""
baseUrlByEnv e         = maybe (error "absurd") buildUrl (subdomainByEnv e)

subdomainByEnv :: Env -> Maybe String
subdomainByEnv Ipfs      = pure "ipfs"
subdomainByEnv Mainnet   = pure "cardano-mainnet"
subdomainByEnv Testnet   = pure "cardano-testnet"
subdomainByEnv Preprod   = pure "cardano-preprod"
subdomainByEnv Preview   = pure "cardano-preview"
subdomainByEnv Localhost = Nothing

-- | Read file according to BLOCKFROST_TOKEN_PATH environment variable name.
projectFromEnv :: IO Project
projectFromEnv = projectFromEnv' "BLOCKFROST_TOKEN_PATH"

-- | Read file according to environment variable name.
projectFromEnv' :: String -> IO Project
projectFromEnv' envVarName = do
  tokPath <- System.Environment.getEnv envVarName
  projectFromFile tokPath

-- | Read file with token and turn it into @Project@
-- Expects tokens prefixed with environment, e.g.
-- @testnetA3C2E...@
projectFromFile :: FilePath -> IO Project
projectFromFile f = mkProject <$> Data.Text.IO.readFile f

data BlockfrostError =
    BlockfrostError Text
  | BlockfrostBadRequest Text   -- 400
  | BlockfrostTokenMissing Text -- 403
  | BlockfrostNotFound          -- 404
  | BlockfrostIPBanned          -- 418
  | BlockfrostMempoolFullOrPinQueueFull -- 425
  | BlockfrostUsageLimitReached -- 429
  | BlockfrostFatal Text        -- 500
  | ServantClientError ClientError
  deriving (Eq, Show)

fromServantClientError :: ClientError -> BlockfrostError
fromServantClientError e = case e of
  FailureResponse _bUrl (Response s _ _ body)
    | s == status400 ->
        BlockfrostBadRequest (withMessage body)
    | s == status403 ->
        BlockfrostTokenMissing (withMessage body)
    | s == status404 ->
        BlockfrostNotFound
    | s == status418 ->
        BlockfrostIPBanned
    | s == mkStatus 425 "Mempool Full (TXs) or Pin Queue Full (IPFS)" ->
        BlockfrostMempoolFullOrPinQueueFull
    | s == status429 ->
        BlockfrostUsageLimitReached
    | s == status500 ->
        BlockfrostFatal (withMessage body)
    | otherwise ->
        BlockfrostError (withMessage body)
  _ -> ServantClientError e
  where
    withMessage body =
      case eitherDecode body of
        (Right (ae :: ApiError)) -> apiErrorMessage ae
        _                        -> mempty

instance ToMultipart Tmp Form where
  toMultipart (Form fileName filePath) =
    MultipartData
      mempty -- no text fields
      [ FileData "file"
                 fileName
                 "application/octet-stream"
                 filePath
      ]
