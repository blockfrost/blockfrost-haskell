-- | Datum Hash newtype

module Blockfrost.Types.Shared.DatumHash
  ( DatumHash (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

-- | Hash of the datum
newtype DatumHash = DatumHash { unDatumHash :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance IsString DatumHash where
  fromString = DatumHash . Data.Text.pack

instance ToJSON DatumHash where
  toJSON = toJSON . unDatumHash
  toEncoding = toEncoding . unDatumHash
instance FromJSON DatumHash where
  parseJSON = fmap DatumHash <$> parseJSON

instance ToSample DatumHash where
    toSamples _ = samples $ map DatumHash
      [ "5a595ce795815e81d22a1a522cf3987d546dc5bb016de61b002edd63a5413ec4"
      , "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
      ]

instance ToCapture (Capture "datum_hash" DatumHash) where
  toCapture _ = DocCapture "datum_hash" "Datum hash."
