{-# LANGUAGE FunctionalDependencies #-}

-- |
--    Module      : Myopia.QuadTree
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Exposes the quad tree data structure
module Myopia.QuadTree
  ( Boundry (..)
  , HasPos (..)
  , Point (..)
  , Quadrant (..)
  , QuadTree
  , V2 (..)
  , divide
  , doesOverlap
  , elemsInBoundry
  , empty
  , inBounds
  , insert
  , insertElems
  )
where

import Graphics.SDL (Point (..), V2 (..))
import Myopia.QuadTree.Internal

-- | Interface for records that have positional data
-- associated with them.
class Num i => HasPos r i | r -> i where
  getPosition :: r -> Point V2 i

empty :: Boundry i -> Int -> QuadTree i a
empty boundry capacity =
  QuadTree
    { region = Leaf []
    , boundry = boundry
    , capacity = capacity
    }

-- | Insert list of elements into a quad tree
insertElems :: (Fractional i, Ord i, HasPos a i) => [a] -> QuadTree i a -> QuadTree i a
insertElems elems qt = foldr insert qt elems

-- | Insert new element into a quad tree
insert :: forall a i. (Fractional i, Ord i, HasPos a i) => a -> QuadTree i a -> QuadTree i a
insert element QuadTree {..} =
  QuadTree {region = addToQuadrant element boundry region, ..}
  where
    addToQuadrant :: a -> Boundry i -> Quadrant [a] -> Quadrant [a]
    addToQuadrant elem boundry (Leaf elems)
      | inBounds elem boundry =
          if length elems < capacity
            then Leaf (elem : elems)
            else
              Node
                (insertElems bNw)
                (insertElems bNe)
                (insertElems bSw)
                (insertElems bSe)
      | otherwise = Leaf elems
      where
        (bNw, bNe, bSw, bSe) = divide boundry
        insertElems bound = foldr (`addToQuadrant` bound) emptyQuad (elem : elems)
    addToQuadrant elem boundry (Node nw ne sw se) =
      Node (addToQuadrant elem bNw nw) (addToQuadrant elem bNe ne) (addToQuadrant elem bSw sw) (addToQuadrant elem bSe se)
      where
        (bNw, bNe, bSw, bSe) = divide boundry
    emptyQuad = Leaf []

-- | Splits a boundry into four smaller boundries (nw, ne, sw, se)
divide :: forall i. Fractional i => Boundry i -> (Boundry i, Boundry i, Boundry i, Boundry i)
divide bound =
  ( split $ P (V2 -halfW halfH)
  , split $ P (V2 halfW halfH)
  , split $ P (V2 -halfW -halfH)
  , split $ P (V2 halfW -halfH)
  )
  where
    split :: Point V2 i -> Boundry i
    split diff = Boundry (bound.center + diff) halfW halfH
    halfW = bound.width / 2
    halfH = bound.height / 2

-- | Inclusive bounds check
inBounds :: (HasPos r i, Ord i) => r -> Boundry i -> Bool
inBounds record (Boundry (P (V2 x y)) w h) =
  px >= (x - w) && px <= (x + w) && py >= (y - h) && py <= (y + h)
  where
    (P (V2 px py)) = getPosition record

doesOverlap :: (Num i, Ord i) => Boundry i -> Boundry i -> Bool
doesOverlap (Boundry (P (V2 x1 y1)) w1 h1) (Boundry (P (V2 x2 y2)) w2 h2) =
  t1 > t2 || b1 < b2 || r1 < r2 || l1 < l2
  where
    t1 = y1 + h1
    t2 = y2 + h2
    b1 = y1 - h1
    b2 = y2 - h2
    r1 = x1 + w1
    r2 = x2 + w2
    l1 = x1 - w1
    l2 = x2 - w2

elemsInBoundry :: forall a i. (Fractional i, Ord i) => QuadTree i a -> Boundry i -> [a]
elemsInBoundry qt = go qt.region qt.boundry
  where
    go :: Quadrant [a] -> Boundry i -> Boundry i -> [a]
    go (Leaf a) quadBound queryBound
      | doesOverlap quadBound queryBound = a
      | otherwise = []
    go (Node nw ne sw se) quadBound queryBound =
      concatMap ($ queryBound) [go nw bNw, go ne bNe, go sw bSw, go se bSe]
      where
        (bNw, bNe, bSw, bSe) = divide quadBound
