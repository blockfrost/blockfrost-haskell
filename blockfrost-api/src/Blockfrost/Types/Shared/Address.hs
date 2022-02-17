-- | Address newtype

module Blockfrost.Types.Shared.Address
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

newtype Address = Address Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

mkAddress :: Text -> Address
mkAddress = Address

unAddress :: Address -> Text
unAddress (Address a) = a

instance IsString Address where
  fromString = mkAddress . Data.Text.pack

instance ToCapture (Capture "address" Address) where
  toCapture _ = DocCapture "address" "Bech32 encoded address"

instance ToCapture (Capture "stake_address" Address) where
  toCapture _ = DocCapture "stake_address" "Bech32 stake address"

instance ToSample Address where
    toSamples = pure $ samples [
        "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
      , "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7"
      ]
