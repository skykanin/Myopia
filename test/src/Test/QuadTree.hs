{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  )
where

import Control.Applicative
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, try)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import GHC.Stack.Types (HasCallStack)
import Myopia.QuadTree
import Myopia.QuadTree.Internal
import Myopia.Util
import Test.QuickCheck
import Test.Sandwich

-- Dummy data type for inserting into quad trees
newtype Position = Pos (Double, Double)
  deriving stock (Show, Generic)

-- Must implement the HasPos interface for quad tree to work
instance HasPos Position Double where
  getPosition (Pos (x, y)) = P (V2 x y)

instance Arbitrary Position where
  arbitrary = liftA2 (Pos ... (,)) arbitrary arbitrary

instance Arbitrary a => Arbitrary (Quadrant a) where
  arbitrary = sized tree
    where
      tree 0 = Leaf <$> arbitrary
      tree n =
        oneof
          [ tree 0
          , Node <$> subtree <*> subtree <*> subtree <*> subtree
          ]
        where
          subtree = tree (n `div` 4)

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance (Num i, Ord i, Arbitrary i) => Arbitrary (Boundry i) where
  arbitrary = Boundry <$> location <*> width <*> height
    where
      location = P ... V2 <$> arbitrary <*> arbitrary
      width = getPositive . Positive <$> arbitrary
      height = getPositive . Positive <$> arbitrary

hasSize :: Int -> Quadrant [a] -> Bool
hasSize n (Leaf xs) = n >= length xs
hasSize n (Node nw ne sw se) = all (hasSize n) [nw, ne, sw, se]

instance (Num i, Ord i, Arbitrary i, Arbitrary a) => Arbitrary (QuadTree i a) where
  arbitrary = do
    capacity <- arbitrary
    region <- arbitrary `suchThat` hasSize capacity
    boundry <- arbitrary
    pure $ QuadTree region boundry capacity

-- TODO: implement property checks
--
-- inserting out of bounds point doesn't change the quad tree
-- insert a qt = qt
--
-- inserting and then querying all in bounds should return the same elements
-- elemsInBoundry (insertElems inbounds qt) = inbounds
--
-- if an element is inbounds then dividing the boundry should only should
-- only cause that element to be inbounds of 1 out of the 4 new boundries
-- inBounds elem boundry = True -> length (filter inBounds (divide boundry)) = 1

-- propInsertOutOfBounds :: Gen Property
-- propInsertOutOfBounds = do
--   randQuadTree <- arbitrary @(QuadTree Int (Boundry Int))
--   bound <- arbitrary @(Boundry Int)
--   let outOfBounds = not . inBounds bound
--       quadTree = randQuadTree {boundry = bound}
--   position <- arbitrary `suchThat` outOfBounds
--   pure $ insert position quadTree === quadTree

-- Test that the inBounds function behaves correctly
testBounds :: (HasCallStack, MonadCatch m) => m ()
testBounds =
  traverse_
    (uncurry $ testBoundry boundry)
    (withRes inside True <> withRes outside False)
  where
    boundry =
      Boundry
        { location = P (V2 100 100)
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
