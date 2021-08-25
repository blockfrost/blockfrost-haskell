{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module NutLink
  where

import Data.Aeson (Value (..), eitherDecode, object, (.=))
import Data.Text (Text)
import qualified Data.Vector
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_nutlink :: Spec
spec_nutlink = do
  it "parses nutlink address sample" $ do
    eitherDecode nutlinkAddressSample
    `shouldBe`
    Right nutlinkAddressExpected

  it "parses nutlink address tickers sample" $ do
    eitherDecode nutlinkAddressTickersSample
    `shouldBe`
    Right nutlinkAddressTickersExpected

  it "parses nutlink ticker sample" $ do
    eitherDecode nutlinkTickerSample
    `shouldBe`
    Right nutlinkTickerExpected

  it "parses nutlink ticker with address sample" $ do
    eitherDecode nutlinkTickerWithAddressSample
    `shouldBe`
    Right nutlinkTickerWithAddressExpected

nutlinkAddressSample = [r|
{
  "address": "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz",
  "metadata_url": "https://nut.link/metadata.json",
  "metadata_hash": "6bf124f217d0e5a0a8adb1dbd8540e1334280d49ab861127868339f43b3948af",
  "metadata": {}
}
|]

nutlinkAddressExpected =
  NutlinkAddress
    { _nutlinkAddressAddress = "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz"
    , _nutlinkAddressMetadataUrl = "https://nut.link/metadata.json"
    , _nutlinkAddressMetadataHash = "6bf124f217d0e5a0a8adb1dbd8540e1334280d49ab861127868339f43b3948af"
    , _nutlinkAddressMetadata = pure $ object []
    }

nutlinkAddressTickersSample = [r|
[
  {
    "name": "ADAUSD",
    "count": 1980038,
    "latest_block": 2657092
  },
  {
    "name": "ADAEUR",
    "count": 1980038,
    "latest_block": 2657092
  },
  {
    "name": "ADABTC",
    "count": 1980038,
    "latest_block": 2657092
  }
]
|]

nutlinkAddressTickersExpected =
  [ NutlinkAddressTicker
      { _nutlinkAddressTickerName = "ADAUSD"
      , _nutlinkAddressTickerCount = 1980038
      , _nutlinkAddressTickerLatestBlock = 2657092
      }
  , NutlinkAddressTicker
      { _nutlinkAddressTickerName = "ADAEUR"
      , _nutlinkAddressTickerCount = 1980038
      , _nutlinkAddressTickerLatestBlock = 2657092
      }
  , NutlinkAddressTicker
      { _nutlinkAddressTickerName = "ADABTC"
      , _nutlinkAddressTickerCount = 1980038
      , _nutlinkAddressTickerLatestBlock = 2657092
      }
  ]


nutlinkTickerSample = [r|
{
  "tx_hash": "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b",
  "block_height": 2657092,
  "tx_index": 8,
  "payload": [
    {
      "source": "coinGecko",
      "value": "1.29"
    },
    {
      "source": "cryptoCompare",
      "value": "1.283"
    }
  ]
}
|]

sampleArray = Array
  $ Data.Vector.fromList
  [ object [ "source" .= ("coinGecko" :: Text)
           , "value" .= ("1.29" :: Text) ]
  , object [ "source" .= ("cryptoCompare" :: Text),
             "value" .= ("1.283" :: Text) ]
  ]

nutlinkTickerExpected =
  NutlinkTicker
    { _nutlinkTickerTxHash = "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b"
    , _nutlinkTickerBlockHeight = 2657092
    , _nutlinkTickerTxIndex = 8
    , _nutlinkTickerPayload = sampleArray
    }

nutlinkTickerWithAddressSample = [r|
{
  "address": "addr_test1qpmtp5t0t5y6cqkaz7rfsyrx7mld77kpvksgkwm0p7en7qum7a589n30e80tclzrrnj8qr4qvzj6al0vpgtnmrkkksnqd8upj0",
  "tx_hash": "e8073fd5318ff43eca18a852527166aa8008bee9ee9e891f585612b7e4ba700b",
  "block_height": 2657092,
  "tx_index": 8,
  "payload": [
    {
      "source": "coinGecko",
      "value": "1.29"
    },
    {
      "source": "cryptoCompare",
      "value": "1.283"
    }
  ]
}
|]

nutlinkTickerWithAddressExpected :: (Address, NutlinkTicker)
nutlinkTickerWithAddressExpected =
  ("addr_test1qpmtp5t0t5y6cqkaz7rfsyrx7mld77kpvksgkwm0p7en7qum7a589n30e80tclzrrnj8qr4qvzj6al0vpgtnmrkkksnqd8upj0"
  , nutlinkTickerExpected)
