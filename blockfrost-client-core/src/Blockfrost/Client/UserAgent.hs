-- | Blockfrost client version via user agent header

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Blockfrost.Client.UserAgent () where

import Blockfrost.Util.UserAgent
import Data.CaseInsensitive (mk)
import Data.Proxy (Proxy (..))
import Data.Sequence ((<|))
import Servant.API ((:>))
import Servant.Client.Core (Client, HasClient (..), requestHeaders)

instance {-# OVERLAPS #-} (HasClient m api) => HasClient m (UserAgent :> api) where
  type Client m (UserAgent :> api) = Client m api

  clientWithRoute m _ req
    = clientWithRoute m (Proxy :: Proxy api)
    $ req { requestHeaders = (mk "User-Agent", userAgent) <| requestHeaders req  }

  hoistClientMonad pm _ nt = hoistClientMonad pm (Proxy :: Proxy api) nt
