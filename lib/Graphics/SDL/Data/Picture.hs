{- |
   Module      : Graphics.SDL.Data.Picture
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the Picture data type which represents what to be rendered and helper functions
-}
module Graphics.SDL.Data.Picture (
  Name,
  Position (..),
  Picture (..),
  SpriteData (..),
  noTransform,
  scaleBy,
) where

import Data.Int (Int16)
import Foreign.C.Types (CDouble, CInt)
import Graphics.SDL.Data.Color (Color)
import SDL (Point, V2 (..))
import SDL.Primitive (Pos, Radius, Width)

type Name = String

-- | The picture to be drawn on the screen
data Picture
  = -- | Draw a line between two points
    Line Pos Pos
  | -- | Draw a line between two points with a width
    ThickLine Pos Pos Width
  | -- | Draw a triangle between three points
    Triangle Pos Pos Pos
  | -- | Draw a rectangle from top left to bottom right corner
    Rect Pos Pos
  | -- | Draw a circle given a point and the radius
    Circle Pos Radius
  | -- | Draw a polygon given a list of tuples representing points
    Polygon [(Int16, Int16)]
  | -- | Draw a filled shape instead of an outline
    Fill Picture
  | -- | Draw a shape in a specific colour
    Color Color Picture
  | -- | Draw a sprite given the sprite name, file location and sprite data
    Sprite Name FilePath SpriteData
  | -- | Draw a picture consisting of several others
    Pictures [Picture]
  deriving (Eq)

-- | Position indicating where to start drawing the sprite from
data Position = TopLeft | BottomLeft | Center
  deriving (Eq, Show)

-- | Describes texture to be rendered
data SpriteData = SpriteData
  { -- | Point to begin the rendering of rectangle
    pos :: Point V2 CInt
  , -- | The factor for upscaling the sprite
    scale :: CInt
  , -- | The point to start drawing the sprite from
    drawFrom :: Position
  , -- | Sprite rotation in degrees
    rotation :: CDouble
  , -- | The point indicating the center of the rotation, or Nothing to rotate around the center of the destination rectangle
    rotationPos :: Maybe (Point V2 CInt)
  , -- | Whether to flip the sprite on its axes
    flipVec :: V2 Bool
  }
  deriving (Eq, Show)

noTransform :: Point V2 CInt -> SpriteData
noTransform position =
{- |
  Create a sprite data context which doesn't do any transformations on the sprite
  Draws from the center by default
-}
  SpriteData
    { pos = position
    , scale = 1
    , drawFrom = Center
    , rotation = 0
    , rotationPos = Nothing
    , flipVec = V2 False False
    }

-- | Scale sprite by some value
scaleBy :: CInt -> SpriteData -> SpriteData
scaleBy size spriteData = spriteData {scale = size}
