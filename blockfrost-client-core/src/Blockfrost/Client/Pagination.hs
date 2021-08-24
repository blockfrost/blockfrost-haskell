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
  ) where

import Data.Proxy (Proxy (..))
import Servant.API ((:>))
import Servant.Client.Core (Client, HasClient (..))

import Blockfrost.Util.Pagination
  ( Paged (..)
  , Pagination
  , PaginationExpanded
  , page
  )

instance HasClient m subApi => HasClient m (Pagination :> subApi) where
    type Client m (Pagination :> subApi) = Paged -> Client m subApi
    clientWithRoute pm _ req paged =
      clientWithRoute
        pm
        (Proxy @(PaginationExpanded subApi))
        req
        (Just $ countPerPage paged)
        (Just $ pageNumber paged)

    hoistClientMonad pm _ hst subCli = hoistClientMonad pm (Proxy @subApi) hst . subCli
