{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.Util.UserAgent (
    UserAgent
  , userAgent
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8
import qualified Data.Version
import qualified Paths_blockfrost_api

data UserAgent

userAgent :: ByteString
userAgent =
  "blockfrost-haskell/"
  <> Data.ByteString.Char8.pack
        (Data.Version.showVersion Paths_blockfrost_api.version)
