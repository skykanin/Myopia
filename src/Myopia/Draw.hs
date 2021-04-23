{-# LANGUAGE RecordWildCards #-}

{- |
   Module      : Myopia.Draw
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module dealing with the drawing of the game state
-}
module Myopia.Draw where

import Graphics.SDL
import Myopia.GameState (Animate (..), GameState (..), Player (..), PlayerState (..), TileType (..))

draw :: GameState -> Picture
draw GameState {..} =
  Pictures
    [ drawRoom floorTex wallTex room
    , drawPlayer player
    ]

drawPlayer :: Player -> Picture
drawPlayer Player {..} =
  let spriteData = scaleBy 4 $ noTransform position
   in case playerState of
        Idle -> Sprite name fp spriteData
          where
            (name, fp) = sprites idle !! currentSprite idle
        Running -> Sprite name fp spriteData
          where
            (name, fp) = sprites running !! currentSprite running

drawRoom :: (Name, FilePath) -> (Name, FilePath) -> [(TileType, SpriteData)] -> Picture
drawRoom (fName, fPath) (wName, wPath) room =
  Pictures $ map (uncurry tile) room
  where
    tile :: TileType -> SpriteData -> Picture
    tile Floor = Sprite fName fPath
    tile Wall = Sprite wName wPath
