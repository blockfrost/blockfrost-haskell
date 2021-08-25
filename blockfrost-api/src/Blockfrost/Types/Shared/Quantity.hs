-- | Quantity wrapper

module Blockfrost.Types.Shared.Quantity
  where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import qualified Data.Text
import GHC.Generics
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample (..), samples)
import qualified Text.Read

newtype Quantity = Quantity Integer
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Read, FromHttpApiData, ToHttpApiData)

unQuantity :: Quantity -> Integer
unQuantity (Quantity i) = i

instance ToJSON Quantity where
  toJSON = toJSON . show . unQuantity
  toEncoding = toEncoding . show . unQuantity

instance FromJSON Quantity where
  parseJSON = withText "quantity" $ \q -> do
    case Text.Read.readMaybe (Data.Text.unpack q) of
      Nothing    -> fail "Unable to read quantity as Integer"
      Just quant -> pure quant

instance ToSample Quantity where
    toSamples = pure $ samples [37040682, 412162133]
