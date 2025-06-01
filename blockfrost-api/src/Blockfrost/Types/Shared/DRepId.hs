-- | DRep identifier

module Blockfrost.Types.Shared.DRepId
  where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

newtype DRepIdBech32 = DRepIdBech32 Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

mkDRepIdBech32 :: Text -> DRepIdBech32
mkDRepIdBech32 = DRepIdBech32

unDRepIdBech32 :: DRepIdBech32 -> Text
unDRepIdBech32 (DRepIdBech32 a) = a

instance IsString DRepIdBech32 where
  fromString = mkDRepIdBech32 . Data.Text.pack

instance ToSample DRepIdBech32 where
    toSamples = pure $ samples
      [ "drep1mvdu8slennngja7w4un6knwezufra70887zuxpprd64jxfveahn"
      , "drep1cxayn4fgy27yaucvhamsvqj3v6835mh3tjjx6x8hdnr4"
      ]

newtype DRepIdHex = DRepIdHex Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

mkDRepIdHex :: Text -> DRepIdHex
mkDRepIdHex = DRepIdHex

unDRepIdHex :: DRepIdHex -> Text
unDRepIdHex (DRepIdHex a) = a

instance IsString DRepIdHex where
  fromString = mkDRepIdHex . Data.Text.pack

instance ToSample DRepIdHex where
    toSamples = pure $ samples
      [ "db1bc3c3f99ce68977ceaf27ab4dd917123ef9e73f85c304236eab23"
      , "c1ba49d52822bc4ef30cbf77060251668f1a6ef15ca46d18f76cc758"
      ]

data DRepId
  = DRepId_Bech32 DRepIdBech32
  | DRepId_Hex DRepIdHex
  deriving stock (Eq, Show, Generic)

instance ToJSON DRepId where
  toJSON (DRepId_Bech32 dRepIdBech32) = toJSON dRepIdBech32
  toJSON (DRepId_Hex dRepIdHex)       = toJSON dRepIdHex

  toEncoding (DRepId_Bech32 dRepIdBech32) = toEncoding dRepIdBech32
  toEncoding (DRepId_Hex dRepIdHex)       = toEncoding dRepIdHex

instance FromJSON DRepId where
  parseJSON = withText "dRepId" $ \case
    x | "drep" `Data.Text.isPrefixOf` x -> pure $ DRepId_Bech32 $ DRepIdBech32 x
    x | otherwise                       -> pure $ DRepId_Hex $ DRepIdHex x

instance ToHttpApiData DRepId where
  toUrlPiece (DRepId_Bech32 x) = toUrlPiece x
  toUrlPiece (DRepId_Hex x)   = toUrlPiece x

instance FromHttpApiData DRepId where
  parseUrlPiece  x | "drep" `Data.Text.isPrefixOf` x = Right $ DRepId_Bech32 (DRepIdBech32 x)
  parseUrlPiece  x | otherwise                       = Right $ DRepId_Hex (DRepIdHex x)

instance ToSample DRepId where
    toSamples = pure $ samples
      [ DRepId_Bech32 "drep1mvdu8slennngja7w4un6knwezufra70887zuxpprd64jxfveahn"
      , DRepId_Bech32 "drep1cxayn4fgy27yaucvhamsvqj3v6835mh3tjjx6x8hdnr4"
      , DRepId_Hex "db1bc3c3f99ce68977ceaf27ab4dd917123ef9e73f85c304236eab23"
      , DRepId_Hex "c1ba49d52822bc4ef30cbf77060251668f1a6ef15ca46d18f76cc758"
      ]

instance ToCapture (Capture "drep_id" DRepId) where
  toCapture _ = DocCapture "drep_id" "Bech32 or hexadecimal DRep ID."
