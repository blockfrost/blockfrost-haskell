-- | Tag instance for HasClient

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Blockfrost.Client.Tag
  where

import Data.Proxy (Proxy (..))
import Servant.API ((:>))
import Servant.Client.Core (Client, HasClient (..))

import Blockfrost.Util.Tag (Tag)

instance HasClient m subApi => HasClient m (Tag name :> subApi) where
    type Client m (Tag name :> subApi) = Client m subApi
    clientWithRoute pm _ = clientWithRoute pm (Proxy @subApi)
    hoistClientMonad pm _ hst = hoistClientMonad pm (Proxy @subApi) hst
