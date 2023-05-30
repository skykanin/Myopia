{- |
   Module      : Graphics.SDL.Internal.DrawState
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the draw state type for rendering
-}
module Graphics.SDL.Internal.DrawState
  ( DrawState (..)
  , startDrawState
  )
where

import Data.IORef (IORef, newIORef)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Graphics.SDL.Data.Color (Color)
import SDL (Texture, V4 (..))
import SDL.Font (Font)

-- | State of the current draw loop
data DrawState = DrawState
  { filled :: Bool
  -- ^ Fill in the drawn picture
  , color :: Color
  -- ^ Color to draw picture in
  , textures :: IORef [(String, Texture)]
  -- ^ Cache of loaded textures
  , fonts :: IORef (Map String Font)
  }
  deriving stock (Eq)

startDrawState :: IO DrawState
startDrawState = do
  textureRef <- newIORef []
  fontRef <- newIORef Map.empty
  pure $
    DrawState
      { filled = False
      , color = V4 0 0 0 255
      , textures = textureRef
      , fonts = fontRef
      }
