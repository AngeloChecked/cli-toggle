
module Foo.MathSpec where

import Test.Hspec

import Foo.Math

spec :: Spec
spec = do
  describe "absolute" $ do
    it "returns the original number when given a positive input" $
      absolute (1::Int) `shouldBe` (1::Int)

    it "returns a positive number when given a negative input" $
      absolute (-1) `shouldBe` 1

    it "returns zero when given zero" $
      absolute 1 `shouldBe` 1