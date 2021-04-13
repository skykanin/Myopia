{- |
   Module      : Graphics.SDL.Internal.DrawState
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the draw state type for rendering
-}
module Graphics.SDL.Internal.DrawState (
  DrawState (..),
  startDrawState,
) where

import Graphics.SDL.Data.Color (Color)
import SDL (V4 (..))

-- | State of the current draw loop
data DrawState = DrawState
  { -- | Fill in the drawn picture
    filled :: Bool
  , -- | Color to draw picture in
    color :: Color
  }
  deriving (Eq, Show)

startDrawState :: DrawState
startDrawState = DrawState {filled = False, color = V4 0 0 0 255}
