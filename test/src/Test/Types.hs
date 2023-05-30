{-# OPTIONS_GHC -Wno-orphans #-}

{- |
   Module      : Test.Types
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
Types and typeclass instances for writing tests
-}
module Test.Types (Position (..)) where

import Control.Applicative (liftA2)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Myopia.QuadTree (Boundary (..), HasPos (..), Point (..), Quadrant (..), V2 (..))
import Myopia.QuadTree.Internal (QuadTree (..))
import Myopia.Util
import Test.QuickCheck

-- Dummy data type for inserting into quad trees
data Position = Pos CInt CInt
  deriving stock (Eq, Generic, Show)

instance HasPos Position Double where
  getPosition (Pos x y) = P (V2 (fromIntegral x) (fromIntegral y))

-------------------- Arbitrary instances for QuickCheck --------------------

instance Arbitrary Position where
  arbitrary = liftA2 Pos arbitrary arbitrary

instance (Arbitrary a) => Arbitrary (Quadrant a) where
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

instance (Arbitrary a) => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance (RealFrac i, Arbitrary i) => Arbitrary (Boundary i) where
  arbitrary = do
    center <- P ... V2 @i <$> arbitrary <*> arbitrary
    Positive @Double width <- arbitrary
    Positive @Double height <- arbitrary
    pure $ Boundary (fromIntegral . round <$> center) (fromIntegral $ ceiling width) (fromIntegral $ ceiling height)

-- | Check that each quadrant doesn't contain more elements than
-- the provided size 'n'.
hasSize :: Int -> Quadrant [a] -> Bool
hasSize n (Leaf xs) = n >= length xs
hasSize n (Node nw ne sw se) = all (hasSize n) [nw, ne, sw, se]

instance (RealFrac i, Arbitrary i, Arbitrary a) => Arbitrary (QuadTree i a) where
  arbitrary = do
    -- ensures that the max capacity is 100 elements
    capacity <- chooseInt (0, 100)
    region <- arbitrary `suchThat` hasSize capacity
    boundary <- arbitrary
    pure $ QuadTree region boundary capacity
