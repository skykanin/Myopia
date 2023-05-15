module Myopia.Draw.Drawable (Drawable (..)) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Graphics.SDL
import Myopia.QuadTree (Boundary (..), Quadrant (..))
import Myopia.QuadTree qualified as QT
import Myopia.QuadTree.Internal (QuadTree (..))
import Myopia.State.Entity (Animation (..), Entity (..), Movement (..))
import Myopia.State.Entity qualified as Entity
import Myopia.State.Room (Room (..))
import Myopia.State.Type (Animate (..))
import Optics.Core

-- | Interface representing types that can be rendered as a @'Picture'@.
class Drawable a where
  draw :: a -> Picture

instance Drawable (Boundary CInt) where
  draw b = Rect topLeft bottomRight
    where
      topLeft = V2 (cx - b.width) (cy - b.height)
      bottomRight = V2 (cx + b.width) (cy + b.height)
      P (V2 cx cy) = b.center

instance Drawable Entity where
  draw entity = Pictures [boundary, entitySprite]
    where
      boundary = draw $ floor @Double @CInt <$> Entity.mkBoundary entity
      entitySprite =
        case entity.movement of
          Idle -> Sprite name fp entity.spriteData
            where
              (name, fp) = entity.animations.idle.sprites !! (entity.animations.idle.currentSprite `div` entity.animations.idle.slowdown)
          Moving -> Sprite name fp entity.spriteData
            where
              (name, fp) = entity.animations.moving.sprites !! (entity.animations.moving.currentSprite `div` entity.animations.moving.slowdown)

instance Drawable Room where
  draw room = Pictures $ V.toList $ fmap toPicture room.layout
    where
      toPicture (tileType, spriteData) = Sprite name filePath spriteData
        where
          (_, (name, filePath)) =
            fromMaybe (error "Texture missing in list") $ find ((==) tileType . fst) room.textures

instance Drawable a => Drawable (QuadTree Double a) where
  draw qt =
    Pictures
      [ Color white $ drawBoundries qt.boundary qt.region
      , Color green $ drawElements qt.region
      ]
    where
      drawElements quad = case quad of
        Leaf xs -> Pictures $ draw <$> xs
        Node nw ne sw se -> Pictures $ map drawElements [nw, ne, sw, se]
      drawBoundries b q = case q of
        Leaf _ -> drawBoundry b
        Node nw ne sw se -> Pictures $ zipWith drawBoundries (QT.divide b ^. partsOf each) [nw, ne, sw, se]
      drawBoundry b@Boundary {center = P (V2 cx cy)} =
        Rect
          (V2 (round $ cx - b.width) (round $ cy + b.height))
          (V2 (round $ cx + b.width) (round $ cy - b.height))
