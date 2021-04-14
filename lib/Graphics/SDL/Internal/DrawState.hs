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

import Data.IORef (IORef, newIORef)
import Graphics.SDL.Data.Color (Color)
import Graphics.SDL.Data.Picture (Name)
import SDL (Texture, V4 (..))

-- | State of the current draw loop
data DrawState = DrawState
  { -- | Fill in the drawn picture
    filled :: Bool
  , -- | Color to draw picture in
    color :: Color
  , -- | Cache of textures that we've loaded
    stateTextures :: IORef [(Name, Texture)]
  }
  deriving (Eq)

startDrawState :: IO DrawState
startDrawState = do
  ref <- newIORef []
  pure $ DrawState {filled = False, color = V4 0 0 0 255, stateTextures = ref}
