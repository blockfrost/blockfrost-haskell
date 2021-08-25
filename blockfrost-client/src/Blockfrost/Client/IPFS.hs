-- | IPFS client functions
{-# LANGUAGE OverloadedStrings #-}

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

ipfsAdd_ :: Project -> (ByteString, Form) -> BlockfrostClient IPFSAdd
ipfsAdd_ = _add . ipfsClient

-- | Add a file or directory to IPFS
ipfsAdd :: FilePath -> BlockfrostClient IPFSAdd
ipfsAdd fp = do
  hasFile <- liftIO $ System.Directory.doesFileExist fp
  if hasFile
    then do
      liftIO $ putStrLn $ "Uploading: " ++ fp
      let fn = Data.Text.pack $ System.FilePath.takeBaseName fp
      go (\proj -> ipfsAdd_ proj ("suchBoundary", (Form fn fp)))
    else
      throwError (BlockfrostError "No such file")

ipfsGateway_ :: Project -> Text -> BlockfrostClient IPFSData
ipfsGateway_ = _gateway . ipfsClient

-- | Fetch file via API
ipfsGateway :: Text -> BlockfrostClient IPFSData
ipfsGateway x = go (`ipfsGateway_` x)

ipfsPin_ :: Project -> Text -> BlockfrostClient IPFSPinChange
ipfsPin_ = _pin . ipfsClient

-- | Pin an object
ipfsPin :: Text -> BlockfrostClient IPFSPinChange
ipfsPin x = go (`ipfsPin_` x)

ipfsListPins_ :: Project -> Paged -> SortOrder -> BlockfrostClient [IPFSPin]
ipfsListPins_ = _listPins . ipfsClient

-- | List objects pinned to local storage
-- Allows custom paging and ordering using @Paged@ and @SortOrder@.
ipfsListPins' :: Paged -> SortOrder -> BlockfrostClient [IPFSPin]
ipfsListPins' pg s = go (\p -> ipfsListPins_ p pg s)

-- | List objects pinned to local storage
ipfsListPins :: BlockfrostClient [IPFSPin]
ipfsListPins = ipfsListPins' def def

ipfsGetPin_ :: Project -> Text -> BlockfrostClient IPFSPin
ipfsGetPin_ = _getPin . ipfsClient

-- | Get pinned object details
ipfsGetPin :: Text -> BlockfrostClient IPFSPin
ipfsGetPin x = go (`ipfsGetPin_` x)

ipfsRemovePin_ :: Project -> Text -> BlockfrostClient IPFSPinChange
ipfsRemovePin_ = _removePin . ipfsClient

-- | Remove pinned object from local storage
ipfsRemovePin :: Text -> BlockfrostClient IPFSPinChange
ipfsRemovePin x = go (`ipfsRemovePin_` x)
