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
draw gs =
  Pictures
    [ drawRoom gs.room
    , drawPlayer gs.player
    ]

drawPlayer :: Player -> Picture
drawPlayer player =
  case player.playerMovement of
    Idle -> Sprite name fp player.spriteData
      where
        (name, fp) = player.idle.sprites !! (player.idle.currentSprite `div` player.idle.animSlowdown)
    Running -> Sprite name fp player.spriteData
      where
        (name, fp) = player.running.sprites !! (player.running.currentSprite `div` player.running.animSlowdown)

drawRoom :: Room -> Picture
drawRoom room = Pictures $ V.toList $ fmap toPicture room.layout
  where
    toPicture (tileType, spriteData) = Sprite name filePath spriteData
      where
        (_, (name, filePath)) =
          fromMaybe (error "Texture missing in list") $ find ((==) tileType . fst) room.textures
