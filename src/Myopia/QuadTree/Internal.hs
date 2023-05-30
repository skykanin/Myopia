{- |
   Module      : Myopia.QuadTree.Internal
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Internal module exposing the types for the quad tree
-}
module Myopia.QuadTree.Internal
  ( Boundary (..)
  , Quadrant (..)
  , QuadTree (..)
  )
where

import GHC.Generics (Generic)
import Graphics.SDL (Point (..), V2 (..))

data Boundary i = Boundary
  { center :: Point V2 i
  -- ^ center of the @'Boundary'@
  , width :: i
  -- ^ width from center to an edge of the @'Boundary'@
  , height :: i
  -- ^ height from center to an edge of the @'Boundary'@
  }
  deriving stock (Functor, Generic, Eq, Show)

data Quadrant a
  = Leaf a
  | Node (Quadrant a) (Quadrant a) (Quadrant a) (Quadrant a)
  deriving stock (Eq, Show, Functor, Generic)

-- | A 'QuadTree' contains a region of quadrants with elements, the boundary of the
-- entire 'QuadTree' and the max capacity of elements for each quadrant. If there is
-- 'Leaf' instead then that's like having one quadrant.
data QuadTree i a = QuadTree
  { region :: Quadrant [a]
  , boundary :: Boundary i
  , capacity :: Int
  }
  deriving stock (Eq, Generic, Show)
