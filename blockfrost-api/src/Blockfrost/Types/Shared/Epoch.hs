-- | Epoch

module Blockfrost.Types.Shared.Epoch
  where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

newtype Epoch = Epoch Integer
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, Real, Integral, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

unEpoch :: Epoch -> Integer
unEpoch (Epoch i) = i

instance ToCapture (Capture "epoch_number" Epoch) where
  toCapture _ = DocCapture "epoch_number" "Epoch for specific epoch slot."

instance ToSample Epoch where
    toSamples = pure $ samples [425, 500, 1200]
