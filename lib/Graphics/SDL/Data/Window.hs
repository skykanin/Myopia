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
  WindowTitle,
  defaultWindow,
) where

import Data.Text (Text)
import SDL.Video (
  WindowConfig (..),
  WindowGraphicsContext (..),
  WindowMode (..),
  defaultWindow,
 )

type WindowTitle = Text
