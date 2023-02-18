-- |
--    Module      : Myopia.QuadTree.Internal
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Internal module exposing the types for the quad tree
module Myopia.QuadTree.Internal
  ( Boundry (..)
  , Quadrant (..)
  , QuadTree (..)
  )
where

import GHC.Generics (Generic)
import Graphics.SDL (Point (..), V2 (..))

data Boundry i = Boundry
  { location :: Point V2 i
  , width :: i
  , height :: i
  }
  deriving stock (Generic, Eq, Show)

data Quadrant a
  = Leaf a
  | Node (Quadrant a) (Quadrant a) (Quadrant a) (Quadrant a)
  deriving stock (Eq, Show, Functor, Generic)

data QuadTree i a = QuadTree
  { region :: Quadrant [a]
  , boundry :: Boundry i
  , capacity :: Int
  }
  deriving stock (Eq, Generic, Show)
