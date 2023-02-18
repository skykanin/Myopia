-- |
--    Module      : Myopia.Draw
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Module dealing with the drawing of the game state
module Myopia.Draw (draw) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Graphics.SDL
import Myopia.State.Game (GameState (..))
import Myopia.State.Player (Player (..), PlayerMovement (..))
import Myopia.State.Room (Room (..))
import Myopia.State.Type (Animate (..))

draw :: GameState -> Picture
draw GameState {..} =
  Pictures
    [ drawRoom room
    , drawPlayer player
    ]

drawPlayer :: Player -> Picture
drawPlayer Player {..} =
  case playerMovement of
    Idle -> Sprite name fp spriteData
      where
        (name, fp) = idle.sprites !! (idle.currentSprite `div` idle.animSlowdown)
    Running -> Sprite name fp spriteData
      where
        (name, fp) = running.sprites !! (running.currentSprite `div` running.animSlowdown)

drawRoom :: Room -> Picture
drawRoom Room {..} = Pictures $ V.toList $ fmap toPicture roomLayout
  where
    toPicture (tileType, spriteData) = Sprite name filePath spriteData
      where
        (_, (name, filePath)) =
          fromMaybe (error "Texture missing in list") $ find ((==) tileType . fst) textures
