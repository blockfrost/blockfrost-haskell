-- | Blockfrost authentication scheme instance for HasClient

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Blockfrost.Client.Auth
  ( Project
  ) where

import Blockfrost.Auth
import Data.CaseInsensitive (mk)
import Data.Proxy (Proxy (..))
import Data.Sequence ((<|))
import GHC.TypeLits
import Servant.API ((:>))
import Servant.Client.Core (Client, HasClient (..), requestHeaders)

import qualified Data.ByteString.Char8
import qualified Data.Text.Encoding

instance {-# OVERLAPS #-} (HasClient m api, KnownSymbol sym) => HasClient m (ProjectAuth '[APIKeyInHeader sym] a :> api) where
  type Client m (ProjectAuth '[APIKeyInHeader sym] a :> api) = Project -> Client m api

  clientWithRoute m _ req proj
    = clientWithRoute m (Proxy :: Proxy api)
    $ req { requestHeaders = (headerName, headerVal) <| requestHeaders req  }
      where
        headerVal = Data.Text.Encoding.encodeUtf8 $ projectId proj
        headerName = mk $ Data.ByteString.Char8.pack $ symbolVal (Proxy @sym)

  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl
