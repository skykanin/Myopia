{- |
   Module      : Myopia.State.Tree
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the Tree and Points in it
-}
module Myopia.State.Tree (initTree) where

import Myopia.QuadTree (Boundary (..), Point (..), QuadTree, V2 (..))
import Myopia.QuadTree qualified as QT

initTree :: QuadTree Double a
initTree = QT.empty boundary 4
  where
    boundary =
      Boundary
        { center = P $ V2 600 400
        , width = 550
        , height = 400
        }
