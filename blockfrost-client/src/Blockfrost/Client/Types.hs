-- | Client Types
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blockfrost.Client.Types
  ( BlockfrostClient
  , BlockfrostError (..)
  , ClientConfig
  , runBlockfrost
  , apiClient
  , api0Client
  , commonClient
  , cardanoClient
  , ipfsClient
  , nutLinkClient
  , Project (..)
  , Paged (..)
  , SortOrder (..)
  , go
  , AsClientT
  , fromServant
  , tryError
  , def
  , BlockfrostClientT (..)
  , MonadBlockfrost (..)
  , runBlockfrostClientT
  , newClientConfig
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Default

import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic

import Blockfrost.API
import Blockfrost.Client.Core

type ClientConfig = (ClientEnv, Project)

newtype BlockfrostClientT m a = BlockfrostClientT {
  unBlockfrostClientT
    :: ExceptT BlockfrostError
        (ReaderT ClientConfig m) a
  } deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader ClientConfig
      , MonadError BlockfrostError
      )

type BlockfrostClient = BlockfrostClientT IO

class MonadIO m => MonadBlockfrost m where
  liftBlockfrostClient :: ClientM a -> m a
  getConf :: m ClientConfig

instance MonadIO m => MonadBlockfrost (BlockfrostClientT m) where
  liftBlockfrostClient act = BlockfrostClientT $ do
    (env, _proj) <- ask
    liftIO (runClientM act env)
      >>= either (throwError . fromServantClientError) pure
  getConf = BlockfrostClientT ask

instance MonadBlockfrost ClientM where
  liftBlockfrostClient = id
  getConf = newClientConfig

instance MonadBlockfrost IO where
  liftBlockfrostClient act = getConf >>= \(env, _prj) -> runClientM act env >>= either (error . show) pure
  getConf = newClientConfig

apiClient :: MonadBlockfrost m => BlockfrostAPI (AsClientT m)
apiClient = genericClientHoist liftBlockfrostClient

api0Client :: MonadBlockfrost m => Project -> BlockfrostV0API (AsClientT m)
api0Client = fromServant . _apiV0 apiClient

-- ** Client runner

-- | Run @BlockfrostClientT@ monad in @IO@, using provided @Project@
runBlockfrost
  :: Project
  -> BlockfrostClientT IO a
  -> IO (Either BlockfrostError a)
runBlockfrost = runBlockfrostClientT

-- | Run @BlockfrostClientT@, using provided @Project@
runBlockfrostClientT
  :: MonadIO m => Project
  -> BlockfrostClientT m a
  -> m (Either BlockfrostError a)
runBlockfrostClientT proj act = do
  env <- liftIO $ newEnvByProject proj
  flip runReaderT (env, proj)
    $ runExceptT $ unBlockfrostClientT act

-- | Build default `ClientConfig` using BLOCKFROST_TOKEN_PATH environment variable
newClientConfig
  :: MonadIO m
  => m ClientConfig
newClientConfig = liftIO $ do
  prj <- projectFromEnv
  env <- newEnvByProject prj
  pure (env, prj)

-- | Helper
go :: MonadBlockfrost m
   => (Project -> m a)
   -> m a
go act = getConf >>= act . snd

-- Until mtl > 2.2.2
-- https://github.com/haskell/mtl/pull/66
#if !MIN_VERSION_mtl(2,3,0)
-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
#endif

-- ** Service clients

commonClient :: MonadBlockfrost m => Project -> CommonAPI (AsClientT m)
commonClient = fromServant . _common . api0Client

cardanoClient :: MonadBlockfrost m => Project -> CardanoAPI (AsClientT m)
cardanoClient = fromServant . _cardano . api0Client

ipfsClient :: MonadBlockfrost m => Project -> IPFSAPI (AsClientT m)
ipfsClient = fromServant . _ipfs . api0Client

nutLinkClient :: MonadBlockfrost m => Project -> NutLinkAPI (AsClientT m)
nutLinkClient = fromServant . _nutLink . api0Client
