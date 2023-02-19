-- |
--    Module      : Main
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Entry point for unit tests
module Main (main) where

import Test.QuadTree
import Test.Sandwich

basic :: TopSpec
basic = do
  describe "Quad tree" $ do
    it "inserts into a tree" $ do
      (2 + 2) `shouldBe` 4
    it "Makes sure that the bounds check works correctly" $ do
      testBounds

main :: IO ()
main = do
  print testTree
  runSandwichWithCommandLineArgs defaultOptions basic
