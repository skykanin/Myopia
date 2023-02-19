-- |
--    Module      : Test.QuadTree
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Unit tests ensuring proper behaviour of the QuadTree data structure
module Test.QuadTree
  ( Position (..)
  , testBounds
  , testTree
  , propInsertOutOfBounds
  )
where

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, try)
import Data.Foldable (traverse_)
import GHC.Stack.Types (HasCallStack)
import Myopia.QuadTree
import Myopia.QuadTree.Internal
import Test.QuickCheck
import Test.Sandwich
import Test.Types

-- TODO: implement property checks
--
-- inserting and then querying all in bounds should return the same elements
-- elemsInBoundry (insertElems inbounds qt) = inbounds
--
-- if an element is inbounds then dividing the boundry should
-- only cause that element to be inbounds of 1 out of the 4 new boundries
-- inBounds elem boundry = True -> length (filter inBounds (divide boundry)) = 1

propInsertOutOfBounds :: Gen Property
propInsertOutOfBounds = do
  quadTree <- arbitrary @(QuadTree Double Position)
  let outOfBounds = not . flip inBounds quadTree.boundry
  position <- arbitrary `suchThat` outOfBounds
  pure $ insert position quadTree === quadTree

-- Test that the inBounds function behaves correctly
testBounds :: (HasCallStack, MonadCatch m) => m ()
testBounds =
  traverse_
    (uncurry $ testBoundry boundry)
    (withRes inside True <> withRes outside False)
  where
    boundry =
      Boundry
        { center = P (V2 100 100)
        , width = 60
        , height = 50
        }
    inside =
      [ Pos (100, 100)
      , Pos (40, 50)
      , Pos (160, 150)
      , Pos (160, 50)
      , Pos (40, 150)
      ]
    outside =
      [ Pos (200, 200)
      , Pos (160, 151)
      , Pos (40, 49)
      , Pos (160, 49)
      , Pos (161, 151)
      ]
    withRes list res = map (,res) list

testBoundry :: forall m. (HasCallStack, MonadCatch m) => Boundry Double -> Position -> Bool -> m ()
testBoundry boundry pos expected = do
  res <- try @m @SomeException (inBounds pos boundry `shouldBe` expected)
  case res of
    Right _ -> pure ()
    Left _ -> expectationFailure $ unwords ["Expected:", show pos, "to be in boundry of", show boundry]

-- Test that the insertElems function behaves correctly
testTree :: QuadTree Double Position
testTree = insertElems positions emptyQT
  where
    emptyQT = emptyTree (Boundry (P (V2 100 100)) 50 50) 4
    positions =
      [ Pos (100, 100)
      , Pos (120, 120)
      , Pos (110, 110)
      , Pos (125, 125)
      , Pos (140, 140)
      ]
