-- | Tag for OpenAPI 3 docs

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.Util.Tag
  ( Tag
  ) where

import GHC.TypeLits (Symbol)

-- | Attaches a tag to documentation.
-- Server / client implementations remains intact.
data Tag (name :: Symbol)
