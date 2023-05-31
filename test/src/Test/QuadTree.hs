{- |
   Module      : Test.QuadTree
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Unit tests ensuring proper behaviour of the QuadTree data structure
-}
module Test.QuadTree
  ( Position (..)
  , testBounds
  , testOverlappingBoundries
  , propInsertOutOfBounds
  , propBoundarySoleOwner
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
import Myopia.Util
import Optics.Core
import Test.QuickCheck
import Test.Sandwich
import Test.Types

-- | Generate a @'Position'@ inside a given @'Boundary'@.
genPosInBoundary :: Boundary Double -> Gen Position
genPosInBoundary bound = Pos <$> pickWidth <*> pickHeight
  where
    P (V2 cx cy) = bound.center
    -- Account for bottom right bias of @'QT.inBounds'@ function.
    pickWidth = ceiling <$> choose (cx - bound.width, cx + bound.width)
    pickHeight = floor <$> choose (cy - bound.height, cy + bound.height)

-- | Inserting and then querying all in bound elements in a @'QuadTree'@
-- returns the elements we started with.
propElemsInBound :: Gen Property
propElemsInBound = do
  boundary <- arbitrary
  positions <- listOf $ genPosInBoundary boundary
  let quadTree = QT.insertElems positions . QT.empty boundary $ length positions
  pure
    . counterexample (unlines [show quadTree, show positions])
    $ QT.elemsInBoundary quadTree quadTree.boundary === positions

-- | An element that's in bound of a boundary 'b' will only
-- be in bounds of exactly one boundary when 'b' is divided into four
-- new sub-boundries.
propBoundarySoleOwner :: Gen Property
propBoundarySoleOwner = do
  boundary <- arbitrary
  position <- genPosInBoundary boundary
  pure
    . counterexample (unlines [show boundary, show position])
    $ length (filter (QT.inBounds position) $ QT.divide boundary ^.. each) === 1

-- | Inserting an out of bounds element into a @'QuadTree'@ doesn't do anything.
propInsertOutOfBounds :: Gen Property
propInsertOutOfBounds = do
  quadTree <- arbitrary @(QuadTree Double Position)
  let outOfBounds = not . flip QT.inBounds quadTree.boundary
  position <- arbitrary `suchThat` outOfBounds
  pure $ QT.insert position quadTree === quadTree

-- Test that the inBounds function behaves correctly
testBounds :: (HasCallStack, MonadCatch m) => m ()
testBounds =
  traverse_
    (uncurry $ testBoundary boundary)
    (withRes inside True <> withRes outside False)
  where
    boundary =
      Boundary
        { center = P (V2 100 100)
        , width = 60
        , height = 50
        }
    inside =
      [ Pos 100 100
      , Pos 160 50
      , Pos 41 149
      , Pos 160 149
      ]
    outside =
      [ Pos 200 200
      , Pos 160 150
      , Pos 160 151
      , Pos 40 49
      , Pos 40 50
      , Pos 160 49
      , Pos 161 151
      , Pos 40 150
      ]
    withRes list res = map (,res) list

testBoundary :: forall m. (HasCallStack, MonadCatch m) => Boundary Double -> Position -> Bool -> m ()
testBoundary boundary pos expected = do
  res <- try @m @SomeException (QT.inBounds pos boundary `shouldBe` expected)
  case res of
    Right _ -> pure ()
    Left _ -> expectationFailure $ unwords ["Expected:", show pos, "to be in boundary of", show boundary]

testOverlappingBoundries :: (MonadCatch m) => m ()
testOverlappingBoundries =
  traverse_ (uncurry3 testOverlap) $
    withExpected overlap True <> withExpected don'tOverlap False
  where
    overlap =
      [ (Boundary {center = P (V2 3 4), width = 5, height = 2}, Boundary {center = P (V2 7 2), width = 2, height = 2})
      , (Boundary {center = P (V2 0 2), width = 2, height = 2}, Boundary {center = P (V2 -3 0), width = 3, height = 2})
      ]
    don'tOverlap =
      [ (Boundary {center = P (V2 0 2), width = 1, height = 2}, Boundary {center = P (V2 -3 0), width = 2, height = 2})
      , (Boundary {center = P (V2 0 2), width = 1, height = 2}, Boundary {center = P (V2 -4 0), width = 3, height = 5})
      ]
    withExpected list expect = map (\(b1, b2) -> (b1, b2, expect)) list

testOverlap :: forall m. (HasCallStack, MonadCatch m) => Boundary Double -> Boundary Double -> Bool -> m ()
testOverlap boundary1 boundary2 expected = do
  res <- try @m @SomeException (QT.doesOverlap boundary1 boundary2 `shouldBe` expected)
  case res of
    Right _ -> pure ()
    Left _ ->
      expectationFailure $
        unwords
          [ "Expected:"
          , show boundary1
          , (if expected then id else ("not " <>)) "to overlap with"
          , show boundary2
          ]
