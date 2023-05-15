{-# LANGUAGE FunctionalDependencies #-}

-- |
--    Module      : Myopia.QuadTree
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Exposes the quad tree data structure
module Myopia.QuadTree
  ( Boundary (..)
  , HasPos (..)
  , Point (..)
  , Quadrant (..)
  , QuadTree
  , V2 (..)
  , divide
  , doesOverlap
  , elemsInBoundary
  , elemsInBoundaryExcept
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

empty :: Boundary i -> Int -> QuadTree i a
empty boundary capacity =
  QuadTree
    { region = Leaf []
    , boundary = boundary
    , capacity = capacity
    }

-- | Insert list of elements into a quad tree
insertElems :: (Fractional i, Ord i, HasPos a i) => [a] -> QuadTree i a -> QuadTree i a
insertElems elems qt = foldr insert qt elems

-- | Insert new element into a quad tree
insert :: forall a i. (Fractional i, Ord i, HasPos a i) => a -> QuadTree i a -> QuadTree i a
insert element QuadTree {..} =
  QuadTree {region = addToQuadrant element boundary region, ..}
  where
    addToQuadrant :: a -> Boundary i -> Quadrant [a] -> Quadrant [a]
    addToQuadrant elem boundary (Leaf elems)
      | inBounds elem boundary =
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
        (bNw, bNe, bSw, bSe) = divide boundary
        insertElems bound = foldr (`addToQuadrant` bound) emptyQuad (elem : elems)
    addToQuadrant elem boundary (Node nw ne sw se) =
      Node (addToQuadrant elem bNw nw) (addToQuadrant elem bNe ne) (addToQuadrant elem bSw sw) (addToQuadrant elem bSe se)
      where
        (bNw, bNe, bSw, bSe) = divide boundary
    emptyQuad = Leaf []

-- | Splits a boundary into four smaller boundries (nw, ne, sw, se)
divide :: forall i. Fractional i => Boundary i -> (Boundary i, Boundary i, Boundary i, Boundary i)
divide bound =
  ( split $ P (V2 -halfW halfH)
  , split $ P (V2 halfW halfH)
  , split $ P (V2 -halfW -halfH)
  , split $ P (V2 halfW -halfH)
  )
  where
    split :: Point V2 i -> Boundary i
    split diff = Boundary (bound.center + diff) halfW halfH
    halfW = bound.width / 2
    halfH = bound.height / 2

-- | Exclusive bounds check with a bias towards including elements
-- in the bottom right corner of a boundary.
inBounds :: (HasPos r i, Ord i) => r -> Boundary i -> Bool
inBounds record (Boundary (P (V2 cx cy)) w h) =
  and [(cx - w) < px, (cx + w) >= px, (cy - h) <= py, (cy + h) > py]
  where
    (P (V2 px py)) = getPosition record

doesOverlap :: (Num i, Ord i) => Boundary i -> Boundary i -> Bool
doesOverlap (Boundary (P (V2 x1 y1)) w1 h1) (Boundary (P (V2 x2 y2)) w2 h2) =
  -- not $ t1 < b2 || t2 < b1 || r1 < l2 || r2 < l1
  t1 > b2 && t2 > b1 && r1 > l2 && r2 > l1
  where
    t1 = y1 + h1
    t2 = y2 + h2
    b1 = y1 - h1
    b2 = y2 - h2
    r1 = x1 + w1
    r2 = x2 + w2
    l1 = x1 - w1
    l2 = x2 - w2

-- TODO: Figure out way to factor out 'go' subroutine in the functions below withow
-- requiring an 'Eq a' constraint on @'elemsInBoundary'@.

elemsInBoundary :: forall a i. (Fractional i, Ord i) => QuadTree i a -> Boundary i -> [a]
elemsInBoundary qt = go qt.region qt.boundary
  where
    go :: Quadrant [a] -> Boundary i -> Boundary i -> [a]
    go (Leaf a) quadBound queryBound
      | doesOverlap quadBound queryBound = a
      | otherwise = []
    go (Node nw ne sw se) quadBound queryBound =
      concatMap ($ queryBound) [go nw bNw, go ne bNe, go sw bSw, go se bSe]
      where
        (bNw, bNe, bSw, bSe) = divide quadBound

-- | Like 'elemnsInBoundary' except also exclude some specific provided element.
elemsInBoundaryExcept :: (Fractional i, Ord i, Eq a) => a -> QuadTree i a -> Boundary i -> [a]
elemsInBoundaryExcept exclude qt = go qt.region qt.boundary
  where
    go (Leaf a) quadBound queryBound
      | doesOverlap quadBound queryBound = filter (/= exclude) a
      | otherwise = []
    go (Node nw ne sw se) quadBound queryBound =
      concatMap ($ queryBound) [go nw bNw, go ne bNe, go sw bSw, go se bSe]
      where
        (bNw, bNe, bSw, bSe) = divide quadBound
