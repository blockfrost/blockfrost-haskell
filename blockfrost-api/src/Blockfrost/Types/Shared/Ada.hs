-- | Lovelaces

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UndecidableInstances #-}

module Blockfrost.Types.Shared.Ada
  ( Lovelaces
  ) where

import Data.Aeson (Encoding, FromJSON (..), ToJSON (..), Value)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol)
import qualified Money
import Servant.Docs (ToSample (..), samples)

type Lovelaces = Money.Discrete "ADA" "lovelace"

instance (KnownSymbol cur, Money.GoodScale sc) => ToSample (Money.Discrete' cur sc) where
  toSamples _ = samples $ map Money.discrete [
      592661
    , 3592661
    , 1337
    , 42
    , 0
    , 3000100
    ]

lovelaceDecimalConfig :: Money.DecimalConf
lovelaceDecimalConfig =
  Money.defaultDecimalConf
    { Money.decimalConf_digits = 0
    , Money.decimalConf_scale =
        Money.scale (Proxy @(Money.UnitScale "ADA" "lovelace"))
    }

instance ToJSON (Money.Discrete' "ADA" '(1_000_000, 1)) where
  toJSON =
      (toJSON :: Text -> Value)
    . Money.discreteToDecimal
        lovelaceDecimalConfig
        Money.Round
  toEncoding =
      (toEncoding :: Text -> Encoding)
    . Money.discreteToDecimal
        lovelaceDecimalConfig
        Money.Round

instance FromJSON (Money.Discrete' "ADA" '(1000000, 1)) where
  parseJSON v = do
     t <- parseJSON v
     maybe
        (fail "Can't parse Discrete ADA value")
        pure
        (Money.discreteFromDecimal lovelaceDecimalConfig t)
