-- | Script Hash newtype

module Blockfrost.Types.Shared.ScriptHash
  ( ScriptHash (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value(..), (.=), (.:))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Vector
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)

-- | Id (hash) of the transaction
newtype ScriptHash = ScriptHash { unScriptHash :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance IsString ScriptHash where
  fromString = ScriptHash . Data.Text.pack

instance ToJSON ScriptHash where
  toJSON = toJSON . unScriptHash
  toEncoding = toEncoding . unScriptHash
instance FromJSON ScriptHash where
  parseJSON = fmap ScriptHash <$> parseJSON

-- Custom instance for list used by script list endpoint
instance {-# OVERLAPS #-} ToJSON [ScriptHash] where
  toJSON = Array . Data.Vector.fromList . map (\sh -> Object ("script_hash" .= (toJSON . unScriptHash $ sh)))
instance {-# OVERLAPS #-} FromJSON [ScriptHash] where
  parseJSON (Array a) = mapM parseJSON' (Data.Vector.toList a)
    where
      parseJSON' (Object b) = b .: "script_hash"
      parseJSON' _          = fail "Unexpected type for ScriptHash"
  parseJSON _         = fail "Expected array for [ScriptHash]"

instance ToSample ScriptHash where
    toSamples _ = samples $ map ScriptHash
      [ "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
      , "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
      ]

instance ToCapture (Capture "script_hash" ScriptHash) where
  toCapture _ = DocCapture "script_hash" "Hash of the script."
