-- | Amount sum type

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Blockfrost.Types.Shared.Amount
  where

import Blockfrost.Types.Shared.Ada (Lovelaces)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (Object)
  , object
  , withObject
  , (.:)
  , (.=)
  )
import GHC.Generics
import qualified Money
import Servant.Docs (ToSample (..), samples)
import qualified Text.Read

-- | Amount, which is either `AdaAmount Lovelaces` representing
-- amount of lovelaces or `AssetAmount SomeDiscrete` for asset amounts,
-- identified by concatenation of asset policy ID
-- and hex-encoded asset_name
data Amount =
    AdaAmount Lovelaces
  | AssetAmount Money.SomeDiscrete
  deriving (Eq, Show, Ord, Generic)

-- | SomeDiscrete values always use scale of 1
unitScale :: Money.Scale
unitScale = case Money.scaleFromRational 1 of
  Just s -> s
  Nothing -> error "Money.scaleFromRational impossible"

instance ToJSON Money.SomeDiscrete where
  toJSON sd =
    object [ "unit" .= Money.someDiscreteCurrency sd
           , "quantity" .= show (Money.someDiscreteAmount sd)
           ]

instance FromJSON Money.SomeDiscrete where
  parseJSON = withObject "amount" $ \o -> do
    u <- o .: "unit"
    (strQuant :: String) <- o .: "quantity"
    case Text.Read.readMaybe strQuant of
      Nothing    -> fail "Unable to read quantity as Integer"
      Just quant -> pure $ Money.mkSomeDiscrete u unitScale quant

instance ToJSON Amount where
  toJSON (AdaAmount lovelaces) =
    object [ "unit" .= ("lovelace" :: String)
           , "quantity" .= toJSON lovelaces
           ]
  toJSON (AssetAmount av) = toJSON av

instance FromJSON Amount where
  parseJSON x@(Object o) = do
    (u :: String) <- o .: "unit"
    v <- o .: "quantity"
    case u of
      "lovelace" -> AdaAmount <$> parseJSON v
      _          -> AssetAmount <$> parseJSON x
  parseJSON other = fail $ "Amount expecting object, got" ++ show other

instance ToSample Amount where
  toSamples = pure $ samples
    [ AdaAmount 42000000
    , AssetAmount
        $ Money.toSomeDiscrete
          (12 :: Money.Discrete'
                    "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                    '(1,1))
    , AssetAmount
        $ Money.toSomeDiscrete
          (18605647 :: Money.Discrete'
                          "6804edf9712d2b619edb6ac86861fe93a730693183a262b165fcc1ba1bc99cad"
                          '(1,1))
    ]

-- | Like @Amount@, extended with @decimals` and `has_nft_onchain_metadata`
data AmountExtended =
    AdaAmountExtended Lovelaces
  | AssetAmountExtended
    { assetAmountExtendedDecimals              :: Maybe Int
    , assetAmountExtendedHasNftOnchainMetadata :: Bool
    , assetAmountExtendedValue                 :: Money.SomeDiscrete
    }
  deriving (Eq, Show, Ord, Generic)

instance ToJSON AmountExtended where
  toJSON (AdaAmountExtended lovelaces) =
    object [ "unit" .= ("lovelace" :: String)
           , "quantity" .= toJSON lovelaces
           , "decimals" .= (6 :: Int)
           , "has_nft_onchain_metadata" .= False
           ]
  toJSON (AssetAmountExtended {..}) =
    object [ "unit" .= Money.someDiscreteCurrency assetAmountExtendedValue
           , "quantity" .= show (Money.someDiscreteAmount assetAmountExtendedValue)
           , "decimals" .= assetAmountExtendedDecimals
           , "has_nft_onchain_metadata" .= assetAmountExtendedHasNftOnchainMetadata
           ]

instance FromJSON AmountExtended where
  parseJSON x@(Object o) = do
    (u :: String) <- o .: "unit"
    v <- o .: "quantity"
    assetAmountExtendedDecimals
      <- o .: "decimals"
    assetAmountExtendedHasNftOnchainMetadata
      <- o .: "has_nft_onchain_metadata"
    case u of
      "lovelace" -> AdaAmountExtended <$> parseJSON v
      _          -> do
        assetAmountExtendedValue <- parseJSON x
        pure AssetAmountExtended{..}

  parseJSON other = fail $ "Amount expecting object, got" ++ show other

instance ToSample AmountExtended where
  toSamples = pure $ samples
    [ AdaAmountExtended 42000000
    , AssetAmountExtended
        { assetAmountExtendedDecimals              = Nothing
        , assetAmountExtendedHasNftOnchainMetadata = True
        , assetAmountExtendedValue =
            Money.toSomeDiscrete
            (12 :: Money.Discrete'
                      "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e"
                      '(1,1))
        }
    ]


