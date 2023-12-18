{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Utils
  where

import Data.Aeson (decode, eitherDecode, encode, object, (.=))
import Data.Text (Text)
import qualified Money
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_scripts :: Spec
spec_scripts = do
  it "parses derived address sample" $ do
    eitherDecode derivedAddressSample
    `shouldBe`
    Right derivedAddressExpected

derivedAddressSample = [r|
{
  "xpub": "d507c8f866691bd96e131334c355188b1a1d0b2fa0ab11545075aab332d77d9eb19657ad13ee581b56b0f8d744d66ca356b93d42fe176b3de007d53e9c4c4e7a",
  "role": 0,
  "index": 0,
  "address": "addr1q90sqnljxky88s0jsnps48jd872p7znzwym0jpzqnax6qs5nfrlkaatu28n0qzmqh7f2cpksxhpc9jefx3wrl0a2wu8q5amen7"
}
|]

derivedAddressExpected =
  DerivedAddress
    { _derivedAddressXpub    = "d507c8f866691bd96e131334c355188b1a1d0b2fa0ab11545075aab332d77d9eb19657ad13ee581b56b0f8d744d66ca356b93d42fe176b3de007d53e9c4c4e7a"
    , _derivedAddressRole    = 0
    , _derivedAddressIndex   = 0
    , _derivedAddressAddress = "addr1q90sqnljxky88s0jsnps48jd872p7znzwym0jpzqnax6qs5nfrlkaatu28n0qzmqh7f2cpksxhpc9jefx3wrl0a2wu8q5amen7"
    }
