-- |  AssetIds

module Blockfrost.Types.Shared.AssetId
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

-- | Concatenation of asset policy ID
-- and hex-encoded asset name
newtype AssetId = AssetId Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

mkAssetId :: Text -> AssetId
mkAssetId = AssetId

unAssetId :: AssetId -> Text
unAssetId (AssetId a) = a

instance IsString AssetId where
  fromString = mkAssetId . Data.Text.pack

instance ToCapture (Capture "asset" AssetId) where
  toCapture _ = DocCapture "asset" "Concatenation of the policy_id and hex-encoded asset_name"

instance ToSample AssetId where
    toSamples = pure $ samples [
        "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
      , "6804edf9712d2b619edb6ac86861fe93a730693183a262b165fcc1ba1bc99cad"
      ]
