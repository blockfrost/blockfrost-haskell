-- | IPFS client functions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Blockfrost.Client.IPFS
  ( ipfsAdd
  , ipfsGateway
  , ipfsGetPin
  , ipfsListPins
  , ipfsListPins'
  , ipfsPin
  , ipfsRemovePin
  ) where

import Blockfrost.API
import Blockfrost.Client.Types
import Blockfrost.Types
import Control.Monad.Except
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text
import qualified System.Directory
import qualified System.FilePath

ipfsAdd_ :: MonadBlockfrost m => Project -> (ByteString, Form) -> m IPFSAdd
ipfsAdd_ = _add . ipfsClient

-- | Add a file or directory to IPFS
ipfsAdd :: (MonadError BlockfrostError m, MonadBlockfrost m) => FilePath -> m IPFSAdd
ipfsAdd fp = do
  hasFile <- liftIO $ System.Directory.doesFileExist fp
  if hasFile
    then do
      liftIO $ putStrLn $ "Uploading: " ++ fp
      let fn = Data.Text.pack $ System.FilePath.takeBaseName fp
      go (\proj -> ipfsAdd_ proj ("suchBoundary", (Form fn fp)))
    else
      throwError (BlockfrostError "No such file")

ipfsGateway_ :: MonadBlockfrost m => Project -> Text -> m IPFSData
ipfsGateway_ = _gateway . ipfsClient

-- | Fetch file via API
ipfsGateway :: MonadBlockfrost m => Text -> m IPFSData
ipfsGateway x = go (`ipfsGateway_` x)

ipfsPin_ ::  MonadBlockfrost m => Project -> Text -> m IPFSPinChange
ipfsPin_ = _pin . ipfsClient

-- | Pin an object
ipfsPin :: MonadBlockfrost m => Text -> m IPFSPinChange
ipfsPin x = go (`ipfsPin_` x)

ipfsListPins_ :: MonadBlockfrost m => Project -> Paged -> SortOrder -> m [IPFSPin]
ipfsListPins_ = _listPins . ipfsClient

-- | List objects pinned to local storage
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
ipfsListPins' :: MonadBlockfrost m => Paged -> SortOrder -> m [IPFSPin]
ipfsListPins' pg s = go (\p -> ipfsListPins_ p pg s)

-- | List objects pinned to local storage
ipfsListPins :: MonadBlockfrost m => m [IPFSPin]
ipfsListPins = ipfsListPins' def def

ipfsGetPin_ :: MonadBlockfrost m => Project -> Text -> m IPFSPin
ipfsGetPin_ = _getPin . ipfsClient

-- | Get pinned object details
ipfsGetPin :: MonadBlockfrost m => Text -> m IPFSPin
ipfsGetPin x = go (`ipfsGetPin_` x)

ipfsRemovePin_ :: MonadBlockfrost m => Project -> Text -> m IPFSPinChange
ipfsRemovePin_ = _removePin . ipfsClient

-- | Remove pinned object from local storage
ipfsRemovePin :: MonadBlockfrost m => Text -> m IPFSPinChange
ipfsRemovePin x = go (`ipfsRemovePin_` x)
