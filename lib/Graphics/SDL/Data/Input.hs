{- |
   Module      : Graphics.SDL.Data.Input
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 The input type for dealing with different user inputs
-}
module Graphics.SDL.Data.Input (
  module SDL.Input.Keyboard.Codes,
  Keysym (..),
) where

import SDL.Input (Keysym (..))
import SDL.Input.Keyboard.Codes
