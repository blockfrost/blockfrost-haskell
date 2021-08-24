-- | Pretty printing for safe-money Lovelace values

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Blockfrost.Pretty.Ada
  ( prettyLovelaces
  , prettyLovelaces'
  , prettyLovelacesDoc
  , prettyLovelacesDoc'
  ) where

import Blockfrost.Pretty.Config
import Blockfrost.Types.Shared.Ada
import Data.Default (Default (def))
import Data.Text (Text)
import qualified Money

import Prettyprinter
import Prettyprinter.Render.Terminal

-- | Pretty print `Lovelaces` using default configuration
prettyLovelaces :: Lovelaces -> Text
prettyLovelaces = prettyLovelaces' def

-- | Pretty print `Lovelaces` using supplied `PrettyConfig`
prettyLovelaces' :: PrettyConfig -> Lovelaces -> Text
prettyLovelaces' PrettyConfig{..} x | x > Money.discrete 1_000_000 =
  Money.denseToDecimal
    pcAdaDecimalConf
    Money.Round
    (Money.denseFromDiscrete x)
  <> " "
  <> prefixIfTestAda pcTestAda
  <> (if pcUnicode then adaSymbol else "ADA")
prettyLovelaces' PrettyConfig{..} x =
  Money.discreteToDecimal
    pcLovelaceDecimalConf
    Money.Round
    x
  <> " "
  <> prefixIfTestAda pcTestAda
  <> "lovelace"
  <> (if x == 1 then mempty else "s")

-- | Pretty print `Lovelaces` to `Doc` using supplied `PrettyConfig`
prettyLovelacesDoc' :: PrettyConfig -> Lovelaces -> Doc AnsiStyle
prettyLovelacesDoc' cfg =
    annotate (color Magenta)
  . pretty
  . prettyLovelaces' cfg

-- | Pretty print `Lovelaces` to `Doc` using default config
prettyLovelacesDoc :: Lovelaces -> Doc AnsiStyle
prettyLovelacesDoc = prettyLovelacesDoc' def

prefixIfTestAda :: Bool -> Text
prefixIfTestAda False = mempty
prefixIfTestAda True  = "t"
