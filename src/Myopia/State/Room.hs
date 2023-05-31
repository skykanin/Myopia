{- |
   Module      : Myopia.State.Room
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the room state and room generation
-}
module Myopia.State.Room (Room (..), TileType (..), startRoom) where

import Data.List (find)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Foreign.C.Types (CInt)
import Graphics.SDL (Point (..), SpriteData, V2 (..))
import Graphics.SDL.Data.Picture (noTransform, scaleBy)

data TileType
  = Floor
  | CrackedFloor
  | LeftWall
  | RightWall
  | TopWall
  | BottomWall
  | TopLeftCorner
  | TopRightCorner
  | BottomLeftCorner
  | BottomRightCorner
  deriving stock (Show, Eq, Ord, Enum)

data Room = Room
  { textures :: [(TileType, (String, FilePath))]
  , spacing :: CInt
  , layout :: Vector (TileType, SpriteData)
  }
  deriving stock (Eq, Show)

textureNames :: [String]
textureNames =
  [ "floor"
  , "cracked floor"
  , "left wall"
  , "right wall"
  , "top wall"
  , "bottom wall"
  , "top left corner"
  , "top right corner"
  , "bottom left corner"
  , "bottom right corner"
  ]

texturePaths :: [FilePath]
texturePaths =
  map
    ("assets/" <>)
    [ "floor_regular.png"
    , "floor_cracked.png"
    , "wall_side_mid_left.png"
    , "wall_side_mid_right.png"
    , "wall_mid.png"
    , "wall_mid.png"
    , "wall_side_mid_left.png"
    , "wall_side_mid_right.png"
    , "wall_side_mid_left.png"
    , "wall_side_mid_right.png"
    ]

startRoom :: Room
startRoom = initRoom (15, 10) 5 16 (16, 16) (40, 40)

initRoom :: (Int, Int) -> CInt -> CInt -> (CInt, CInt) -> (CInt, CInt) -> Room
initRoom (width, height) scale spacing spriteSize startPoint =
  Room
    { textures = zip [Floor .. BottomRightCorner] $ zip textureNames texturePaths
    , spacing = spacing
    , layout = mkRoom (width, height) scale spacing spriteSize startPoint
    }

-- | Generate corner indecies given the room dimensions
genCorners :: (Int, Int) -> [(TileType, Int)]
genCorners (width, height) = zip [TopLeftCorner .. BottomRightCorner] [0, 0 + zeroWidth, end - zeroWidth, end]
  where
    zeroWidth = width - 1
    end = (width * height) - 1

toTiles :: (Int, Int) -> Int -> SpriteData -> (TileType, SpriteData)
toTiles (width, height) index spriteData
  | Just (tileType, _) <- find eqIdx corners = (tileType, spriteData)
  | index `mod` width == 0 = (LeftWall, spriteData)
  | (index + 1) `mod` width == 0 = (RightWall, spriteData)
  | index `elem` top = (TopWall, spriteData)
  | index `elem` bottom = (BottomWall, spriteData)
  | otherwise = (Floor, spriteData)
  where
    eqIdx x = index == snd x
    corners = genCorners (width, height)
    top = [0 .. (width - 1)]
    bottom = [lastIdx - (width - 1) .. lastIdx]
    lastIdx = width * height - 1

mkRoom :: (Int, Int) -> CInt -> CInt -> (CInt, CInt) -> (CInt, CInt) -> Vector (TileType, SpriteData)
mkRoom t@(width, height) scale spacing spriteSize initP@(sX, _) = V.imap (toTiles t) spriteDataList
  where
    spriteDataList = V.unfoldrExactN (width * height) f initP
    inc = scale * spacing
    len = fromIntegral (width - 1)
    f :: (CInt, CInt) -> (SpriteData, (CInt, CInt))
    f (x, y)
      | xInc /= 0 && xInc `mod` (inc * len) == 0 = (spriteData, (sX, y + inc))
      | otherwise = (spriteData, (x + inc, y))
      where
        spriteData = scaleSpriteBy scale (x, y) spriteSize
        xInc = x - sX

scaleSpriteBy :: CInt -> (CInt, CInt) -> (CInt, CInt) -> SpriteData
scaleSpriteBy scale (posX, posY) (width, height) =
  scaleBy scale $ noTransform (P (V2 posX posY)) (V2 width height)
