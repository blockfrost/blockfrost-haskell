-- | Client Types
{-# LANGUAGE CPP #-}

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
type BlockfrostClient =
  ExceptT BlockfrostError
    (ReaderT ClientConfig IO)

apiClient :: BlockfrostAPI (AsClientT BlockfrostClient)
apiClient = genericClientHoist nt
  where nt :: ClientM a -> BlockfrostClient a
        nt act = do
          (env, _proj) <- ask
          liftIO (runClientM act env)
            >>= either (throwError . fromServantClientError) pure

api0Client :: Project -> BlockfrostV0API (AsClientT BlockfrostClient)
api0Client = fromServant . _apiV0 apiClient

-- ** Client runner

-- | Run @BlockfrostClient@ monad, using provided @Project@
runBlockfrost :: Project
  -> BlockfrostClient a
  -> IO (Either BlockfrostError a)
runBlockfrost proj act = do
  env <- newEnvByProject proj
  flip runReaderT (env, proj)
    $ runExceptT act

-- | Helper
go :: (Project -> BlockfrostClient a)
  -> BlockfrostClient a
go act = ask >>= act  . snd

-- Until mtl > 2.2.2
-- https://github.com/haskell/mtl/pull/66
#if !MIN_VERSION_mtl(2,3,0)
-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
#endif

-- ** Service clients

commonClient :: Project -> CommonAPI (AsClientT BlockfrostClient)
commonClient = fromServant . _common . api0Client

cardanoClient :: Project -> CardanoAPI (AsClientT BlockfrostClient)
cardanoClient = fromServant . _cardano . api0Client

ipfsClient :: Project -> IPFSAPI (AsClientT BlockfrostClient)
ipfsClient = fromServant . _ipfs . api0Client

nutLinkClient :: Project -> NutLinkAPI (AsClientT BlockfrostClient)
nutLinkClient = fromServant . _nutLink . api0Client
