-- | Client Types
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}

module Blockfrost.Client.Types
  ( BlockfrostClient
  , BlockfrostError (..)
  , ClientConfig
  , defaultRetryPolicy
  , endlessRetryPolicy
  , defaultRetryJudge
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
import Control.Retry
import Data.Default

import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic

import Blockfrost.API
import Blockfrost.Client.Core

data ClientConfig =
  ClientConfig
    { clientConfigClientEnv   :: ClientEnv
    , clientConfigProject     :: Project
    , clientConfigRetryPolicy :: RetryPolicy
    , clientConfigRetryJudge  :: RetryStatus -> BlockfrostError -> RetryAction
    }

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = exponentialBackoff (1000 * 1000) <> limitRetries 5

endlessRetryPolicy :: RetryPolicy
endlessRetryPolicy = exponentialBackoff (1000 * 1000)

defaultRetryJudge
  :: RetryStatus
  -> BlockfrostError
  -> RetryAction
defaultRetryJudge _retryStatus err = retriableError err

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
    clientConfig <- ask
    liftIO
      $ withRetry
          clientConfig
          $ runClientM act (clientConfigClientEnv clientConfig)
    >>= either
          (throwError . fromServantClientError)
          pure
  getConf = BlockfrostClientT ask

withRetry
  :: ClientConfig
  -> IO (Either ClientError a)
  -> IO (Either ClientError a)
withRetry ClientConfig{..} act =
  retryingDynamic
    clientConfigRetryPolicy
    (\retryStatus -> \case
        Right{} -> pure DontRetry
        Left err ->
          pure
          $ clientConfigRetryJudge
              retryStatus
              (fromServantClientError err)
    )
    (const act)

instance MonadBlockfrost ClientM where
  liftBlockfrostClient = id
  getConf = newClientConfig

instance MonadBlockfrost IO where
  liftBlockfrostClient act =
    getConf
      >>= \clientConfig ->
        withRetry
          clientConfig
          ( runClientM
              act
              (clientConfigClientEnv clientConfig)
          )
        >>= either
              (error . show)
              pure
  getConf = newClientConfig

apiClient
  :: MonadBlockfrost m
  => BlockfrostAPI (AsClientT m)
apiClient = genericClientHoist liftBlockfrostClient

api0Client
  :: MonadBlockfrost m
  => Project
  -> BlockfrostV0API (AsClientT m)
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
  :: MonadIO m
  => Project
  -> BlockfrostClientT m a
  -> m (Either BlockfrostError a)
runBlockfrostClientT proj act = do
  cc <- liftIO $ mkClientConfig proj
  flip runReaderT cc
    $ runExceptT $ unBlockfrostClientT act

-- | Build default `ClientConfig` using BLOCKFROST_TOKEN_PATH environment variable
newClientConfig
  :: MonadIO m
  => m ClientConfig
newClientConfig =
  liftIO
    $ projectFromEnv >>= mkClientConfig

mkClientConfig
  :: MonadIO m
  => Project
  -> m ClientConfig
mkClientConfig prj = do
  env <- liftIO $ newEnvByProject prj
  pure
    $ ClientConfig
        { clientConfigClientEnv   = env
        , clientConfigProject     = prj
        , clientConfigRetryPolicy = defaultRetryPolicy
        , clientConfigRetryJudge  = defaultRetryJudge
        }

-- | Helper
go :: MonadBlockfrost m
   => (Project -> m a)
   -> m a
go act = getConf >>= act . clientConfigProject

-- Until mtl > 2.2.2
-- https://github.com/haskell/mtl/pull/66
#if !MIN_VERSION_mtl(2,3,0)
-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
#endif

-- ** Service clients

commonClient
  :: MonadBlockfrost m
  => Project
  -> CommonAPI (AsClientT m)
commonClient = fromServant . _common . api0Client

cardanoClient
  :: MonadBlockfrost m
  => Project
  -> CardanoAPI (AsClientT m)
cardanoClient = fromServant . _cardano . api0Client

ipfsClient
  :: MonadBlockfrost m
  => Project
  -> IPFSAPI (AsClientT m)
ipfsClient = fromServant . _ipfs . api0Client

nutLinkClient
  :: MonadBlockfrost m
  => Project
  -> NutLinkAPI (AsClientT m)
nutLinkClient = fromServant . _nutLink . api0Client
