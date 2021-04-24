{-# LANGUAGE RecordWildCards #-}

{- |
   Module      : Myopia.State.Player
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the player state and related functions
-}
module Myopia.State.Player (
  Player (..),
  PlayerMovement (..),
  initPlayer,
  iteratePlayer,
) where

import Graphics.SDL (CInt, Name, Point (P), V2 (V2))
import Myopia.State.Type (Animate (..))

data PlayerMovement = Idle | Running
  deriving (Eq, Show)

data Player = Player
  { idle :: Animate
  , running :: Animate
  , playerMovement :: PlayerMovement
  , position :: Point V2 CInt
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
    , playerMovement = Idle
    , position = P (V2 600 300)
    }

iteratePlayer :: Player -> Player
iteratePlayer player@Player {..} =
  case playerMovement of
    Idle -> player {idle = idle {currentSprite = (currentSprite idle + 1) `mod` length (sprites idle)}}
    Running -> player {running = running {currentSprite = (currentSprite running + 1) `mod` length (sprites running)}}
