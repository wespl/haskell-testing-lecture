module Main where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  it "" $ () `shouldBe` ()
