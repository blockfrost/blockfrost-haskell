-- | Epoch
module Blockfrost.Types.Shared.Epoch where

import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
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

newtype EpochLength = EpochLength Word64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, Real, Integral, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

unEpochLength :: EpochLength -> Word64
unEpochLength (EpochLength u) = u

instance ToCapture (Capture "epoch_length" EpochLength) where
  toCapture _ = DocCapture "epoch_length" "Epoch size in a specific Cardano era."

instance ToSample EpochLength where
  toSamples = pure $ samples [21600, 86400, 432000]
