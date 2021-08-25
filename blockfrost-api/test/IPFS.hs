{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module IPFS
  where

import Data.Aeson (eitherDecode)
import Data.Text (Text)
import Test.Hspec
import Test.Tasty.Hspec
import Text.RawString.QQ

import Blockfrost.Types

spec_ipfs :: Spec
spec_ipfs = do
  it "parses add sample" $ do
    eitherDecode pinAddSample
    `shouldBe`
    Right pinAddExpected

  it "parses change sample" $ do
    eitherDecode pinChangeSample
    `shouldBe`
    Right pinChangeExpected

  it "parses pin sample" $ do
    eitherDecode pinSample
    `shouldBe`
    Right pinExpected

pinAddSample = [r|
{
  "name": "README.md",
  "ipfs_hash": "QmZbHqiCxKEVX7QfijzJTkZiSi3WEVTcvANgNAWzDYgZDr",
  "size": "125297"
}
|]

pinAddExpected =
  IPFSAdd
    "README.md"
    "QmZbHqiCxKEVX7QfijzJTkZiSi3WEVTcvANgNAWzDYgZDr"
    125297

pinChangeSample = [r|
{
  "ipfs_hash": "QmPojRfAXYAXV92Dof7gtSgaVuxEk64xx9CKvprqu9VwA8",
  "state": "queued"
}
|]

pinChangeExpected =
  IPFSPinChange
    "QmPojRfAXYAXV92Dof7gtSgaVuxEk64xx9CKvprqu9VwA8"
    Queued

pinSample = [r|
{
  "time_created": 1615551024,
  "time_pinned": 1615551024,
  "ipfs_hash": "QmdVMnULrY95mth2XkwjxDtMHvzuzmvUPTotKE1tgqKbCx",
  "size": "1615551024",
  "state": "pinned"
}
|]

pinExpected =
  IPFSPin
    { _ipfsPinTimeCreated = 1615551024
    , _ipfsPinTimePinned = 1615551024
    , _ipfsPinIpfsHash = "QmdVMnULrY95mth2XkwjxDtMHvzuzmvUPTotKE1tgqKbCx"
    , _ipfsPinSize = 1615551024
    , _ipfsPinState = Pinned
    }
