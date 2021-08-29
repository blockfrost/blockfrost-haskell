-- | Pagination utilities

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.Util.Pagination
  ( Paged (..)
  , Pagination
  , PaginationExpanded
  , page
  , paged
  , nextPage
  , maxPageSize
  ) where

import Data.Default.Class
import Servant.API (QueryParam, (:>))

-- | Pagination parameters
data Paged = Paged
  { countPerPage :: Int -- ^ Count of results per page
  , pageNumber   :: Int -- ^ Page number
  } deriving (Eq, Ord, Show)

-- | Maximum number of items per page
maxPageSize :: Int
maxPageSize = 100

instance Default Paged where
  def = Paged maxPageSize 1

-- | Default `Paged` at specific page number
page :: Int -> Paged
page n | n >= 1 = def { pageNumber = n }
page _ = error "Page number not in range [1..]"

-- | Construct `Paged` from page size and page number.
--
-- Throws error on invalid values.
paged :: Int -> Int -> Paged
paged size _ | size > maxPageSize = error "Page size exceeds 100"
paged _ n | n < 1 = error "Page number not in range [1..]"
paged size n = Paged size n

-- | Increment page number
nextPage :: Paged -> Paged
nextPage p = p { pageNumber = 1 + pageNumber p }

-- | Adds Pagination to an API
data Pagination

type PaginationExpanded subApi =
     QueryParam "count" Int
  :> QueryParam "page" Int
  :> subApi
