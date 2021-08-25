-- | Types for IPFS servics

module Blockfrost.Types.IPFS
  ( IPFSAdd (..)
  , IPFSPinChange (..)
  , IPFSPin (..)
  , PinState (..)
  , IPFSData (..)
  ) where

import Blockfrost.Types.Shared
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Servant.Docs (ToSample (..), singleSample)

-- | IPFS Add response
data IPFSAdd = IPFSAdd
  { _ipfsAddName     :: Text -- ^ Name of the file
  , _ipfsAddIpfsHash :: Text -- ^ IPFS hash of the file
  , _ipfsAddSize     :: Quantity -- ^ Size of the IPFS node
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_ipfsAdd", CamelToSnake]] IPFSAdd

instance ToSample IPFSAdd where
  toSamples = pure $ singleSample $
    IPFSAdd
      "README.md"
      "QmZbHqiCxKEVX7QfijzJTkZiSi3WEVTcvANgNAWzDYgZDr"
      125297

-- | State of the pinned object,
-- which is @Queued@ when we are retriving object.
--
-- If this is successful the state is changed to @Pinned@ or @Failed@ if not.
-- The state @Gc@ means the pinned item has been garbage collected
-- due to account being over storage quota or after it has been
-- moved to @Unpinned@ state by removing the object pin.
data PinState = Queued | Pinned | Unpinned | Failed | Gc
  deriving (Eq, Show, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ConstructorTagModifier '[ToLower]] PinState


-- | IPFS Pin Add response
data IPFSPinChange = IPFSPinChange
  { _ipfsPinChangeIpfsHash :: Text -- ^ IPFS hash of pinned object
  , _ipfsPinChangeState    :: PinState -- ^ State of the pin action
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_ipfsPinChange", CamelToSnake]] IPFSPinChange

instance ToSample IPFSPinChange where
  toSamples = pure $ singleSample $
    IPFSPinChange
      "QmPojRfAXYAXV92Dof7gtSgaVuxEk64xx9CKvprqu9VwA8"
      Queued

-- | IPFS Pin information
data IPFSPin = IPFSPin
  { _ipfsPinTimeCreated :: POSIXTime -- ^ Creation time of the IPFS object on our backends
  , _ipfsPinTimePinned  :: POSIXTime -- ^ Pin time of the IPFS object on our backends
  , _ipfsPinIpfsHash    :: Text -- ^ IPFS hash of the pinned object
  , _ipfsPinSize        :: Quantity -- ^ Size of the IPFS node
  , _ipfsPinState       :: PinState -- ^ State of the pinned object
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_ipfsPin", CamelToSnake]] IPFSPin

instance ToSample IPFSPin where
  toSamples = pure $ singleSample $
    IPFSPin
      { _ipfsPinTimeCreated = 1615551024
      , _ipfsPinTimePinned = 1615551024
      , _ipfsPinIpfsHash = "QmdVMnULrY95mth2XkwjxDtMHvzuzmvUPTotKE1tgqKbCx"
      , _ipfsPinSize = 1615551024
      , _ipfsPinState = Pinned
      }

newtype IPFSData = IPFSData ByteString
  deriving (Show, Eq, Generic)

instance ToSample IPFSData where
  toSamples = pure $ singleSample $ IPFSData "sample ipfs bytestring"

instance MimeRender OctetStream IPFSData where
  mimeRender _ (IPFSData cs) = cs

instance MimeUnrender OctetStream IPFSData where
  mimeUnrender _ lbs = pure $ IPFSData lbs

instance MimeRender PlainText IPFSData where
  mimeRender _ (IPFSData cs) = cs

instance MimeUnrender PlainText IPFSData where
  mimeUnrender _ lbs = pure $ IPFSData lbs
