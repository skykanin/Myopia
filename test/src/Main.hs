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
import Test.Sandwich.QuickCheck

basic :: TopSpec
basic = do
  describe "Quad tree tests" $ do
    it
      "Ensures that the bounds check works correctly"
      testBounds
    introduceQuickCheck $ do
      prop "inserting out of bounds elements doesn't change the quad tree" propInsertOutOfBounds
      prop "element only belongs to one boundry when it's divided into four new boundries" propBoundrySoleOwner

main :: IO ()
main = do
  print testTree
  runSandwichWithCommandLineArgs defaultOptions basic
