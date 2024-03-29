{- |
   Module      : Graphics.SDL.Data.Picture
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the Picture data type which represents what to be rendered and helper functions
-}
module Graphics.SDL.Data.Picture
  ( Position (..)
  , Picture (..)
  , SpriteData (..)
  , noTransform
  , scaleBy
  )
where

import Data.Int (Int16)
import Data.Text (Text)
import Foreign.C.Types (CDouble, CInt)
import GHC.Generics (Generic)
import Graphics.SDL.Data.Color (Color)
import SDL (Point, V2 (..))
import SDL.Font (Style)
import SDL.Primitive (Pos, Radius, Width)

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
  | -- | Draw text given a font filepath, font-size, style, rectangle size, text to render and position
    Text FilePath CInt (Maybe Style) (V2 CInt) Text (Point V2 CInt)
  | -- | Draw a sprite given the sprite name, file location and sprite data
    Sprite String FilePath SpriteData
  | -- | Draw a picture consisting of several others
    Pictures [Picture]
  deriving stock (Eq)

-- | Position indicating where to start drawing the sprite from
data Position
  = TopLeft
  | BottomLeft
  | Center
  deriving stock (Eq, Show)

-- | Describes texture to be rendered
data SpriteData = SpriteData
  { pos :: Point V2 CInt
  -- ^ Point to begin the rendering of rectangle
  , drawFrom :: Position
  -- ^ The point to start drawing the sprite from
  , size :: V2 CInt
  -- ^ The size of the sprite in pixels
  , rotation :: CDouble
  -- ^ Sprite rotation in degrees
  , rotationPos :: Maybe (Point V2 CInt)
  -- ^ The point indicating the center of the rotation, or Nothing to rotate around the center of the destination rectangle
  , flipVec :: V2 Bool
  -- ^ Whether to flip the sprite on its axes
  }
  deriving stock (Generic, Eq, Show)

-- | Create a sprite data context which doesn't do any transformations on the sprite.
-- Draws from the center by default
noTransform :: Point V2 CInt -> V2 CInt -> SpriteData
noTransform position size =
  SpriteData
    { pos = position
    , drawFrom = Center
    , size = size
    , rotation = 0
    , rotationPos = Nothing
    , flipVec = V2 False False
    }

-- | Scale sprite by some value
scaleBy :: CInt -> SpriteData -> SpriteData
scaleBy scale spriteData = spriteData {size = (scale *) <$> spriteData.size}
