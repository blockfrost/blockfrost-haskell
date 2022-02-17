-- | Slot wrapper

module Blockfrost.Types.Shared.Slot
  where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

newtype Slot = Slot Integer
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, Real, Integral, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

unSlot :: Slot -> Integer
unSlot (Slot i) = i

instance ToCapture (Capture "slot_number" Slot) where
  toCapture _ = DocCapture "slot_number" "Slot position for requested block."

instance ToSample Slot where
    toSamples = pure $ samples [37040682, 412162133]
