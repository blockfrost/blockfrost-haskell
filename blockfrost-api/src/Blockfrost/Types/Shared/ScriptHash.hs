-- | Script Hash newtype

module Blockfrost.Types.Shared.ScriptHash
  ( ScriptHash (..)
  , ScriptHashList (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value(..), (.=), (.:))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Vector
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples, singleSample)

-- | Script Hash newtype
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

-- | Wrapper for list of ScriptHash-es, used by script list endpoint
newtype ScriptHashList = ScriptHashList { unScriptHashList :: [ScriptHash] }
  deriving stock (Show, Eq, Generic)

instance ToJSON ScriptHashList where
  toJSON =
      Array
    . Data.Vector.fromList
    . map (\sh -> Object ("script_hash" .= (toJSON . unScriptHash $ sh)))
    . unScriptHashList
instance FromJSON ScriptHashList where
  parseJSON (Array a) = ScriptHashList <$> mapM parseJSON' (Data.Vector.toList a)
    where
      parseJSON' (Object b) = b .: "script_hash"
      parseJSON' _          = fail "Unexpected type for ScriptHash"
  parseJSON _         = fail "Expected array for [ScriptHash]"

instance ToSample ScriptHashList where
  toSamples = pure $ singleSample $ ScriptHashList $ map ScriptHash
    [ "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
    , "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
    ]

instance ToSample ScriptHash where
  toSamples _ = samples $ map ScriptHash
    [ "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
    , "e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"
    ]

instance ToCapture (Capture "script_hash" ScriptHash) where
  toCapture _ = DocCapture "script_hash" "Hash of the script."
