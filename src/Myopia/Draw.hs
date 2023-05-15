-- |
--    Module      : Myopia.Draw
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Module dealing with the drawing of the game state
module Myopia.Draw (drawGameState) where

import Graphics.SDL
import Myopia.Draw.Drawable (Drawable (draw))
import Myopia.State.Game (GameState (..))

drawGameState :: GameState -> Picture
drawGameState gs =
  Pictures
    [ draw gs.room
    , draw gs.quadTree
    ]
