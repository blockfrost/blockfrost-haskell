-- | CBOR Servant support and wrapper type

module Blockfrost.Types.Shared.CBOR
  where

import Data.ByteString.Lazy (ByteString)
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))
import Servant.Docs (ToSample (..), singleSample)

data CBOR

-- | Wrapper for CBOR encoded `ByteString`s
-- used for submitting a transaction
newtype CBORString = CBORString ByteString
  deriving stock (Eq, Show)

instance Accept CBOR where
  contentType = pure "application/cbor"

instance MimeRender CBOR CBORString where
  mimeRender _ (CBORString cs) = cs

instance MimeUnrender CBOR CBORString where
  mimeUnrender _ lbs = pure $ CBORString lbs

instance ToSample CBORString where
  toSamples = pure $ singleSample $ CBORString "adef"
