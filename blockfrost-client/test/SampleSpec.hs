{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SampleSpec
  where

import Test.Hspec
import Test.Tasty.Hspec

spec_sample :: Spec
spec_sample = do
  it "has tests" $ do
    True `shouldBe` True

--main :: IO ()
--main = hspec spec
