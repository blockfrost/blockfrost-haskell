-- | Hash of the block

module Blockfrost.Types.Shared.BlockHash
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isDigit)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Servant.API (Capture, FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), samples)
import qualified Text.Read

newtype BlockHash = BlockHash Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

mkBlockHash :: Text -> BlockHash
mkBlockHash = BlockHash

unBlockHash :: BlockHash -> Text
unBlockHash (BlockHash a) = a

instance IsString BlockHash where
  fromString = mkBlockHash . Data.Text.pack

instance ToCapture (Capture "block_hash" BlockHash) where
  toCapture _ = DocCapture "block_hash" "Specific block hash"

instance ToSample BlockHash where
    toSamples = pure $ samples
      [ "d0fa315687e99ccdc96b14cc2ea74a767405d64427b648c470731a9b69e4606e"
      , "38bc6efb92a830a0ed22a64f979d120d26483fd3c811f6622a8c62175f530878"
      , "f3258fcd8b975c061b4fcdcfcbb438807134d6961ec278c200151274893b6b7d"
      ]

instance ToCapture (Capture "hash_or_number" (Either Integer BlockHash)) where
  toCapture _ = DocCapture "hash_or_number" "Hash or number of the requested block."

instance {-# OVERLAPS #-} ToHttpApiData (Either Integer BlockHash) where
  toUrlPiece = either (Data.Text.pack . show) unBlockHash

instance {-# OVERLAPS #-} FromHttpApiData (Either Integer BlockHash) where
  parseUrlPiece x | Data.Text.all isDigit x =
    case Text.Read.readMaybe (Data.Text.unpack x) of
        Nothing      -> Left "Unable to read block id"
        Just blockId -> pure (Left blockId)
  parseUrlPiece x | otherwise = pure (Right (BlockHash x))
