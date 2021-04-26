{- |
   Module      : Myopia.State.Type
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining commonly used types in the game state
-}
module Myopia.State.Type (
  Animate (..),
  MoveDir (..),
  Sprite,
) where

import Graphics.SDL (Name)

type Sprite = (Name, FilePath)

-- | Data type representing an animation in the rendering loop
data Animate = Animate
  { -- | The sprites in the loop
    sprites :: [Sprite]
  , -- | The sprite we are currently rendering
    currentSprite :: Int
  , -- | The rate at which to slow down the rendering speed
    animSlowdown :: Int
  }
  deriving (Show)

data MoveDir = Up | Down | Left | Right
  deriving (Eq, Show)
