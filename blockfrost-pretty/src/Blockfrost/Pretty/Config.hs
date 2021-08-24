-- | Pretty printing config

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Blockfrost.Pretty.Config
  ( PrettyConfig (..)
  , adaDecimalConf
  , adaSymbol
  , lovelaceDecimalConf
  , testnetConfig
  ) where

import Data.Default (Default (def))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Money

-- | Ada Unicode symbol
adaSymbol :: Text
adaSymbol = "₳"

-- | Pretty printing configuration
data PrettyConfig = PrettyConfig {
    pcTestAda             :: Bool -- ^ Set if working with test Ada
  , pcUnicode             :: Bool -- ^ Set to use unicode symbol for Ada
  , pcAdaDecimalConf      :: Money.DecimalConf -- ^ Decimal config for printing values over 1 Ada
  , pcLovelaceDecimalConf :: Money.DecimalConf -- ^ Decimal config for printing lovelace values (<1 Ada)
  }

instance Default PrettyConfig where
  def =
    PrettyConfig {
      pcTestAda = False
    , pcUnicode = True
    , pcAdaDecimalConf = adaDecimalConf
    , pcLovelaceDecimalConf = lovelaceDecimalConf
    }

-- | `PrettyConfig` for testnet
testnetConfig :: PrettyConfig
testnetConfig = def { pcTestAda = True }

-- | Decimal config for printing Ada value
adaDecimalConf :: Money.DecimalConf
adaDecimalConf =
  Money.defaultDecimalConf
    { Money.decimalConf_digits = 6
    , Money.decimalConf_separators = Money.separatorsDotNarrownbsp
    , Money.decimalConf_scale =
        Money.scale (Proxy @(Money.UnitScale "ADA" "ada"))
    }

-- | Decimal config for printing lovelace values
lovelaceDecimalConf :: Money.DecimalConf
lovelaceDecimalConf =
  Money.defaultDecimalConf
    { Money.decimalConf_digits = 0
    , Money.decimalConf_separators = Money.separatorsDotNarrownbsp
    , Money.decimalConf_scale =
        Money.scale (Proxy @(Money.UnitScale "ADA" "lovelace"))
    }
