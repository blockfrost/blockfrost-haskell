-- | BlockIndex query parameter

module Blockfrost.Types.Shared.BlockIndex
  where

import qualified Data.Text
import GHC.Generics
import Servant.API (FromHttpApiData (..), QueryParam, ToHttpApiData (..))
import Servant.Docs
  ( DocQueryParam (..)
  , ParamKind (..)
  , ToParam (..)
  , ToSample (..)
  , samples
  )

-- | Block height (number) and optional index
data BlockIndex = BlockIndex {
    blockIndexHeight :: Integer
  , blockIndexIndex  :: Maybe Integer
  }
  deriving stock (Eq, Show, Generic)

instance ToHttpApiData BlockIndex where
  toUrlPiece bi = Data.Text.pack $
       show (blockIndexHeight bi)
    <> maybe mempty ((':':) .show) (blockIndexIndex bi)

instance FromHttpApiData BlockIndex where
  parseUrlPiece x = case Data.Text.splitOn ":" x of
    [""] -> Left "Empty block index"
    [bh] -> (`BlockIndex` Nothing) <$> parseUrlPiece bh
    [bh, idx] -> BlockIndex <$> parseUrlPiece bh <*> (Just <$> parseUrlPiece idx)
    _ -> Left "Invalid block index"

instance ToParam (QueryParam "from" BlockIndex) where
  toParam _ =
    DocQueryParam
      "blockIndex"
      []
      "The block number and optionally also index from which (inclusive)\
      \ to start search for results, concatenated using colon.\
      \ Has to be lower than or equal to `to` parameter."
      Normal

instance ToParam (QueryParam "to" BlockIndex) where
  toParam _ =
    DocQueryParam
      "blockIndex"
      []
      "The block number and optionally also index from which (inclusive)\
      \ to end the search for results, concatenated using colon.\
      \ Has to be higher than or equal to `from` parameter."
      Normal

instance ToSample BlockIndex where
    toSamples = pure $ samples [
        BlockIndex 892961 Nothing
      , BlockIndex 9999269 (Just 10)
      ]
