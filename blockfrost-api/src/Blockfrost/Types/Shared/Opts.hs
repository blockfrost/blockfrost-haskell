-- | Aeson options and helpers

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.Types.Shared.Opts
  ( ToLower
  , aesonOptions
  ) where

import Data.Aeson (Options (..), camelTo2, defaultOptions)
import Data.Char (toLower)
import Deriving.Aeson (StringModifier (..))

data ToLower

instance StringModifier ToLower where
  getStringModifier ""       = ""
  getStringModifier (c : xs) = toLower c : xs

aesonOptions :: Maybe String -> Options
aesonOptions mPrefix = defaultOptions {
   fieldLabelModifier = camelTo2 '_'  . dropIfPrefixed
 , constructorTagModifier = map toLower
 }
 where dropIfPrefixed = maybe id (drop . length) mPrefix
