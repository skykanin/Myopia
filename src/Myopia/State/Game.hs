{-# LANGUAGE RecordWildCards #-}

{- |
   Module      : Myopia.State.Game
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the overall game state and game state iteration
-}
module Myopia.State.Game (
  GameState (..),
  TileType (..),
  iterateWorld,
  startState,
) where

import Data.List (mapAccumL)
import Graphics.SDL (CInt, Point (P), SpriteData, V2 (V2), noTransform, scaleBy)
import Myopia.State.Player (Player (..), initPlayer, iteratePlayer)
import Myopia.State.Type (Sprite)

data TileType = Floor | Wall
  deriving (Show, Eq)

data GameState = GameState
  { player :: Player
  , floorTex :: Sprite
  , wallTex :: Sprite
  , room :: [(TileType, SpriteData)]
  }
  deriving (Show)

startState :: GameState
startState =
  GameState
    { player = initPlayer
    , floorTex = ("floor", "assets/floor_1.png")
    , wallTex = ("wall", "assets/wall_mid.png")
    , room = mkRoom 5 (200, 0)
    }

iterateWorld :: GameState -> GameState
iterateWorld gs@GameState {..} = gs {player = iteratePlayer player}

mkRoom :: CInt -> (CInt, CInt) -> [(TileType, SpriteData)]
mkRoom scale startPos@(sX, _) = snd . mapAccumL f startPos . concat $ tenByTenRoom
  where
    inc = scale * 16
    l = fromIntegral (length tenByTenRoom) - 1
    f (x, y) el
      | xInc /= 0 && xInc `mod` (inc * l) == 0 = ((sX, y + inc), (el, spriteData scale x y))
      | otherwise = ((x + inc, y), (el, spriteData scale x y))
      where
        xInc = x - sX

spriteData :: CInt -> CInt -> CInt -> SpriteData
spriteData scale posX posY = scaleBy scale $ noTransform (P (V2 posX posY))

tenByTenRoom :: [[TileType]]
tenByTenRoom = wall 10 : replicate 8 (space 10) ++ [wall 10]
  where
    wall n = replicate n Wall
    space n = Wall : replicate (n - 2) Floor ++ [Wall]
