{- |
   Module      : Graphics.SDL.Data.Color
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the colour data type and some default colours
-}
module Graphics.SDL.Data.Color (
  Color,
  black,
  white,
  red,
  green,
  blue,
  orange,
  pink,
) where

import Data.Word (Word8)
import SDL (V4 (..))

-- The colour represented vector consisting of RGBA values
type Color = V4 Word8

black :: Color
black = V4 0 0 0 255

white :: Color
white = V4 255 255 255 255

red :: Color
red = V4 255 0 0 255

green :: Color
green = V4 0 255 0 255

blue :: Color
blue = V4 0 0 255 255

orange :: Color
orange = V4 255 69 0 255

pink :: Color
pink = V4 255 193 203 255
