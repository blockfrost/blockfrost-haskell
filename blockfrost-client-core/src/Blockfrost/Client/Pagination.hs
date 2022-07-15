-- | Pagination instance for HasClient

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Blockfrost.Client.Pagination
  ( Paged (..)
  , page
  , paged
  , nextPage
  , allPages
  ) where

import Data.Default (Default (def))
import Data.Proxy (Proxy (..))
import Servant.API ((:>))
import Servant.Client.Core (Client, HasClient (..))

import Blockfrost.Util.Pagination
  ( Paged (..)
  , Pagination
  , PaginationExpanded
  , page
  , paged
  , maxPageSize
  , nextPage
  )

-- | Query all results, until we get less than maximum
-- items per page.
allPages :: Monad m => (Paged -> m [a]) -> m [a]
allPages act = do
  let fetch page' = do
        res <- act page'
        case res of
          xs | length xs < maxPageSize -> pure xs
          xs -> do
            next <- fetch (nextPage page')
            pure $ xs ++ next
  fetch def

instance HasClient m subApi => HasClient m (Pagination :> subApi) where
    type Client m (Pagination :> subApi) = Paged -> Client m subApi
    clientWithRoute pm _ req paged' =
      clientWithRoute
        pm
        (Proxy @(PaginationExpanded subApi))
        req
        (Just $ countPerPage paged')
        (Just $ pageNumber paged')

    hoistClientMonad pm _ hst subCli = hoistClientMonad pm (Proxy @subApi) hst . subCli
