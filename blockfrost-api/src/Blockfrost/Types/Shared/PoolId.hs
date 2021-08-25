-- | Pool identifier

module Blockfrost.Types.Shared.PoolId
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

newtype PoolId = PoolId Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

mkPoolId :: Text -> PoolId
mkPoolId = PoolId

unPoolId :: PoolId -> Text
unPoolId (PoolId a) = a

instance IsString PoolId where
  fromString = mkPoolId . Data.Text.pack

instance ToCapture (Capture "pool_id" PoolId) where
  toCapture _ = DocCapture "pool_id" "Specific pool_id"

instance ToSample PoolId where
    toSamples = pure $ samples [
        "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy"
      ]
