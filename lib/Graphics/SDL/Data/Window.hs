{- |
   Module      : Graphics.SDL.Window
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 The window configuration type for handling window parameters
-}
module Graphics.SDL.Data.Window (
  WindowConfig (..),
  WindowMode (..),
  WindowGraphicsContext (..),
  defaultWindow,
) where

import SDL.Video (
  WindowConfig (..),
  WindowGraphicsContext (..),
  WindowMode (..),
  defaultWindow,
 )
