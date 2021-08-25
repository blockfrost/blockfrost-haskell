-- | PolicyId

module Blockfrost.Types.Shared.PolicyId
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

-- | Minting policy Id
newtype PolicyId = PolicyId Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

mkPolicyId :: Text -> PolicyId
mkPolicyId = PolicyId

unPolicyId :: PolicyId -> Text
unPolicyId (PolicyId a) = a

instance IsString PolicyId where
  fromString = mkPolicyId . Data.Text.pack

instance ToCapture (Capture "policy_id" PolicyId) where
  toCapture _ = DocCapture "policy_id" "Specific policy_id"

instance ToSample PolicyId where
    toSamples = pure $ samples [
        "476039a0949cf0b22f6a800f56780184c44533887ca6e821007840c3"
      ]
