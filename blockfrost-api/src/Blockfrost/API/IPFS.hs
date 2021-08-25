-- | IPFS services

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.API.IPFS
  where

import Blockfrost.Types
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting
import Blockfrost.Util.Tag (Tag)
import Data.Text (Text)
import Servant.API
import Servant.API.Generic
import Servant.Docs (DocCapture (..), ToCapture (..))
import Servant.Multipart.API

data IPFSAPI route =
  IPFSAPI
    {
      _add
        :: route
        :- Summary "Add a file or directory to IPFS"
        :> Description "You need to `/ipfs/pin/add` an object to avoid it being garbage collected. \
                        \This usage is being counted in your user account quota."
        :> Tag "IPFS » Add"
        :> "add"
        :> MultipartForm Tmp Form
        :> Post '[JSON] IPFSAdd
    , _gateway
        :: route
        :- Summary "Relay to an IPFS gateway"
        :> Description "Retrieve an object from the IFPS gateway. \
                        \(Useful if you do not want to rely on a public gateway, such as ``ipfs.blockfrost.dev`)."
        :> Tag "IPFS » Gateway"
        :> "gateway"
        :> Capture "IPFS_path" Text
        :> Get '[PlainText, OctetStream] IPFSData
    , _pin
        :: route
        :- Summary "Pin an object"
        :> Description "Pinned objects are counted in your user storage quota."
        :> Tag "IPFS » Pins"
        :> "pin"
        :> "add"
        :> Capture "IPFS_path" Text
        :> Post '[JSON] IPFSPinChange
    , _listPins
        :: route
        :- Summary "List pinned objects"
        :> Description "List objects pinned to local storage."
        :> Tag "IPFS » Pins"
        :> "pin"
        :> "list"
        :> Pagination
        :> Sorting
        :> Get '[JSON] [IPFSPin]
    , _getPin
        :: route
        :- Summary "Get pinned object details"
        :> Description "Obtain inormation about specific pinned object."
        :> Tag "IPFS » Pins"
        :> "pin"
        :> "list"
        :> Capture "IPFS_path" Text
        :> Get '[JSON] IPFSPin
    , _removePin
        :: route
        :- Summary "Remove pinned object from local storage"
        :> Description "Remove pinned object from local storage"
        :> Tag "IPFS » Pins"
        :> "pin"
        :> "remove"
        :> Capture "IPFS_path" Text
        :> Post '[JSON] IPFSPinChange
    } deriving (Generic)

instance ToCapture (Capture "IPFS_path" Text) where
  toCapture _ = DocCapture "IPFS_path" "Path to the IPFS object"

data Form = Form {
    formFileName :: Text
  , formFilePath :: FilePath
  }
