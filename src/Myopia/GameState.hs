{-# LANGUAGE RecordWildCards #-}

{- |
   Module      : Myopia.GameState
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the game state and game state iteration
-}
module Myopia.GameState (
  Animate (..),
  GameState (..),
  Player (..),
  PlayerState (..),
  TileType (..),
  iterateWorld,
  startState,
) where

import Data.List (mapAccumL)
import Graphics.SDL (CInt, Name, Point (P), SpriteData, V2 (V2), noTransform, scaleBy)

data TileType = Floor | Wall
  deriving (Show, Eq)

type Sprite = (Name, FilePath)

data Animate = Animate
  { sprites :: [Sprite]
  , currentSprite :: Int
  }
  deriving (Show)

data PlayerState = Idle | Running
  deriving (Eq, Show)

data Player = Player
  { idle :: Animate
  , running :: Animate
  , playerState :: PlayerState
  , position :: Point V2 CInt
  }
  deriving (Show)

data GameState = GameState
  { player :: Player
  , floorTex :: Sprite
  , wallTex :: Sprite
  , room :: [(TileType, SpriteData)]
  }
  deriving (Show)

idleNames :: [Name]
idleNames = ["wizard_f_idle_f0", "wizard_f_idle_f1", "wizard_f_idle_f2", "wizard_f_idle_f3"]

idleSprites :: [FilePath]
idleSprites = ["wizzard_f_idle_anim_f0.png", "wizzard_f_idle_anim_f1.png", "wizzard_f_idle_anim_f2.png", "wizzard_f_idle_anim_f3.png"]

runNames :: [Name]
runNames = ["wizard_f_run_f0", "wizard_f_run_f1", "wizard_f_run_f2", "wizard_f_run_f3"]

runSprites :: [FilePath]
runSprites = ["wizzard_f_run_anim_f0.png", "wizzard_f_run_anim_f1.png", "wizzard_f_run_anim_f2.png", "wizzard_f_run_anim_f3.png"]

withAssetPath :: [FilePath] -> [FilePath]
withAssetPath = map ("assets/" <>)

initPlayer :: Player
initPlayer =
  Player
    { idle =
        Animate
          { sprites = zip idleNames (withAssetPath idleSprites)
          , currentSprite = 0
          }
    , running =
        Animate
          { sprites = zip runNames (withAssetPath runSprites)
          , currentSprite = 0
          }
    , playerState = Idle
    , position = P (V2 600 300)
    }

startState :: GameState
startState =
  GameState
    { player = initPlayer
    , floorTex = ("floor", "assets/floor_1.png")
    , wallTex = ("wall", "assets/wall_mid.png")
    , room = mkRoom 5 (200, 0)
    }

iteratePlayer :: Player -> Player
iteratePlayer player@Player {..} =
  case playerState of
    Idle -> player {idle = idle {currentSprite = (currentSprite idle + 1) `mod` length (sprites idle)}}
    Running -> player {running = running {currentSprite = (currentSprite running + 1) `mod` length (sprites running)}}

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
