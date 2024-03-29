-- | CBOR Servant support and wrapper type

module Blockfrost.Types.Shared.CBOR
  where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.ByteString.Lazy (ByteString)
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))
import Servant.Docs (ToSample (..), singleSample)

import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.Text

data CBOR

-- | Wrapper for CBOR encoded `ByteString`s
-- used for submitting a transaction
newtype CBORString = CBORString ByteString
  deriving stock (Eq, Show)

instance ToJSON CBORString where
  toJSON (CBORString bs) =
    toJSON
      . Data.Text.pack
      $ Data.ByteString.Char8.unpack
      $ Data.ByteString.Lazy.toStrict bs

instance FromJSON CBORString where
  parseJSON = withText "CBORString" $ \t ->
    pure
      $ CBORString
      <$> Data.ByteString.Lazy.fromStrict
        . Data.ByteString.Char8.pack
        $ Data.Text.unpack t

instance Accept CBOR where
  contentType = pure "application/cbor"

instance MimeRender CBOR CBORString where
  mimeRender _ (CBORString cs) = cs

instance MimeUnrender CBOR CBORString where
  mimeUnrender _ lbs = pure $ CBORString lbs

instance ToSample CBORString where
  toSamples = pure $ singleSample $ CBORString "adef"
