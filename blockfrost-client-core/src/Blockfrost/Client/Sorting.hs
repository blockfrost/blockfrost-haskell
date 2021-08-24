-- | Sorting instance for HasClient

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Blockfrost.Client.Sorting
  ( SortOrder (..)
  , asc
  , desc
  ) where

import Data.Proxy (Proxy (..))
import Servant.API ((:>))
import Servant.Client.Core (Client, HasClient (..))

import Blockfrost.Util.Sorting
  ( SortOrder (..)
  , Sorting
  , SortingExpanded
  , asc
  , desc
  )

instance HasClient m subApi => HasClient m (Sorting :> subApi) where
    type Client m (Sorting :> subApi) = SortOrder -> Client m subApi
    clientWithRoute pm _ req sOrder =
      clientWithRoute
        pm
        (Proxy @(SortingExpanded subApi))
        req
        (Just sOrder)

    hoistClientMonad pm _ hst subCli = hoistClientMonad pm (Proxy @subApi) hst . subCli
