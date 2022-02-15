-- | Transaction Id

module Blockfrost.Types.Shared.TxHash
  ( TxHash (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

-- | Id (hash) of the transaction
newtype TxHash = TxHash { unTxHash :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance IsString TxHash where
  fromString = TxHash . Data.Text.pack

instance ToJSON TxHash where
  toJSON = toJSON . unTxHash
  toEncoding = toEncoding . unTxHash
instance FromJSON TxHash where
  parseJSON = fmap TxHash <$> parseJSON

instance ToSample TxHash where
    toSamples _ = samples $ map TxHash
      [ "8788591983aa73981fc92d6cddbbe643959f5a784e84b8bee0db15823f575a5b"
      , "52e748c4dec58b687b90b0b40d383b9fe1f24c1a833b7395cdf07dd67859f46f"
      , "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b"
      ]

instance ToCapture (Capture "hash" TxHash) where
  toCapture _ = DocCapture "hash" "Hash of the requested transaction."
