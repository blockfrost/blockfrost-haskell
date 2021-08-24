-- | Sorting utilities

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.Util.Sorting
  ( SortOrder (..)
  , Sorting
  , SortingExpanded
  , asc
  , desc
  ) where

import Data.Default.Class
import Servant.API (FromHttpApiData (..), QueryParam, ToHttpApiData (..), (:>))

data SortOrder = Ascending | Descending
  deriving (Eq, Show, Ord)

-- | @Ascending@ shortcut
asc :: SortOrder
asc = Ascending

-- | @Descending@ shortcut
desc :: SortOrder
desc = Descending

instance Default SortOrder where
  def = Ascending

instance ToHttpApiData SortOrder where
  toUrlPiece Ascending  = "asc"
  toUrlPiece Descending = "desc"

instance FromHttpApiData SortOrder where
  parseUrlPiece "asc"  = Right Ascending
  parseUrlPiece "desc" = Right Descending
  parseUrlPiece z      = Left ("unknown SortOrder: " <> z)

-- | Adds sorting to an API
data Sorting

-- | Adds sorting to an API
type SortingExpanded a =
  QueryParam "order" SortOrder
  :> a
