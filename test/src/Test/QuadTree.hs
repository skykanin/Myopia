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
  , propBoundrySoleOwner
  , propElemsInBound
  )
where

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, try)
import Data.Foldable (traverse_)
import GHC.Stack.Types (HasCallStack)
import Myopia.QuadTree (Point (..), V2 (..))
import Myopia.QuadTree qualified as QT
import Myopia.QuadTree.Internal
import Optics.Core
import Test.QuickCheck
import Test.Sandwich
import Test.Types

-- | Generate a @'Position'@ inside a given @'Boundry'@.
genPosInBoundry :: Boundry Double -> Gen Position
genPosInBoundry bound = Pos <$> pick cx bound.width <*> pick cy bound.height
  where
    P (V2 cx cy) = bound.center
    pick c dim = choose (c - dim, c + dim)

-- | Inserting and then querying all in bound elements in a @'QuadTree'@
-- returns the elements we started with.
propElemsInBound :: Gen Property
propElemsInBound = do
  boundry <- arbitrary
  positions <- listOf $ genPosInBoundry boundry
  let quadTree = QT.empty boundry (length positions)
  pure
    . counterexample (unlines [show boundry, show positions])
    $ QT.elemsInBoundry (QT.insertElems positions quadTree) quadTree.boundry === positions

-- | An element that's in bound of a boundry 'b' will only
-- be in bounds of exactly one boundry when 'b' is divided into four
-- new boundries.
propBoundrySoleOwner :: Gen Property
propBoundrySoleOwner = do
  boundry <- arbitrary
  position <- genPosInBoundry boundry
  pure
    . counterexample (unlines [show boundry, show position])
    $ length (filter (QT.inBounds position) $ QT.divide boundry ^.. each) === 1

-- | Inserting an out of bounds element into a @'QuadTree'@ doesn't do anything.
propInsertOutOfBounds :: Gen Property
propInsertOutOfBounds = do
  quadTree <- arbitrary @(QuadTree Double Position)
  let outOfBounds = not . flip QT.inBounds quadTree.boundry
  position <- arbitrary `suchThat` outOfBounds
  pure $ QT.insert position quadTree === quadTree

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
      [ Pos 100 100
      , Pos 40 50
      , Pos 160 150
      , Pos 160 50
      , Pos 40 150
      ]
    outside =
      [ Pos 200 200
      , Pos 160 151
      , Pos 40 49
      , Pos 160 49
      , Pos 161 151
      ]
    withRes list res = map (,res) list

testBoundry :: forall m. (HasCallStack, MonadCatch m) => Boundry Double -> Position -> Bool -> m ()
testBoundry boundry pos expected = do
  res <- try @m @SomeException (QT.inBounds pos boundry `shouldBe` expected)
  case res of
    Right _ -> pure ()
    Left _ -> expectationFailure $ unwords ["Expected:", show pos, "to be in boundry of", show boundry]

-- Test that the insertElems function behaves correctly
testTree :: QuadTree Double Position
testTree = QT.insertElems positions emptyQT
  where
    emptyQT = QT.empty (Boundry (P (V2 100 100)) 50 50) 4
    positions =
      [ Pos 100 100
      , Pos 120 120
      , Pos 110 110
      , Pos 125 125
      , Pos 140 140
      ]
