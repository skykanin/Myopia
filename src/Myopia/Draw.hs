-- |
--    Module      : Myopia.Draw
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Module dealing with the drawing of the game state
module Myopia.Draw (DebugMode (..), Drawable (draw)) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Graphics.SDL
import Myopia.QuadTree (Boundary (..), Quadrant (..))
import Myopia.QuadTree qualified as QT
import Myopia.QuadTree.Internal (QuadTree (..))
import Myopia.State.Entity (Animation (..), Entity (..), Movement (..))
import Myopia.State.Entity qualified as Entity
import Myopia.State.Game (GameState (..))
import Myopia.State.Room (Room (..))
import Myopia.State.Type (Animate (..))
import Optics.Core
import Myopia.Util qualified as Util

-- | Whether or not to enabe the debugging mode where metadata
-- is drawn around different game objects.
data DebugMode = Enabled | Disabled
  deriving stock (Eq, Show)

-- | Interface representing types that can be rendered as a @'Picture'@.
class Drawable a where
  draw :: DebugMode -> a -> Picture

whenEnabled :: Monoid m => DebugMode -> m -> m
whenEnabled debugMode value = case debugMode of
  Enabled -> value
  Disabled -> mempty

instance Drawable GameState where
  draw debugMode gs =
    Pictures
      [ draw debugMode gs.room
      , draw debugMode gs.quadTree
      ]

instance Drawable (Boundary CInt) where
  draw _debugMode b = Rect topLeft bottomRight
    where
      topLeft = V2 (cx - b.width) (cy - b.height)
      bottomRight = V2 (cx + b.width) (cy + b.height)
      P (V2 cx cy) = b.center

instance Drawable Entity where
  draw debugMode entity = Pictures $ entitySprite : debug
    where
      debug = whenEnabled debugMode
        [ draw debugMode $ floor @Double @CInt <$> Entity.mkBoundary entity
        , drawText (V2 80 20) displayPosition bottom
        , drawText (V2 40 20) displayMoveState $ bottom + P (V2 0 20)
        ]
      drawText = Text (Util.withFontPath "joystix monospace.otf") 20 Nothing
      displayMoveState = Util.showt entity.movement
      displayPosition = let V2 x y = pos in "Pos:" <> Util.showt (x, y)
      bottom = P $ pos + V2 -x y
        where
          V2 x y = (`div` 2) <$> entity.spriteData.size
      P pos = entity.spriteData.pos
      entitySprite =
        case entity.movement of
          Idle -> Sprite name fp entity.spriteData
            where
              (name, fp) = entity.animations.idle.sprites !! (entity.animations.idle.currentSprite `div` entity.animations.idle.slowdown)
          Moving -> Sprite name fp entity.spriteData
            where
              (name, fp) = entity.animations.moving.sprites !! (entity.animations.moving.currentSprite `div` entity.animations.moving.slowdown)

instance Drawable Room where
  draw _debugMode room = Pictures $ V.toList $ fmap toPicture room.layout
    where
      toPicture (tileType, spriteData) = Sprite name filePath spriteData
        where
          (_, (name, filePath)) =
            fromMaybe (error "Texture missing in list") $ find ((==) tileType . fst) room.textures

instance Drawable a => Drawable (QuadTree Double a) where
  draw debugMode qt =
    Pictures $
      boundries
        <> [Color green $ drawElements qt.region]
    where
      boundries = whenEnabled debugMode [Color green $ drawBoundries qt.boundary qt.region]
      drawElements quad = case quad of
        Leaf xs -> Pictures $ draw debugMode <$> xs
        Node nw ne sw se -> Pictures $ map drawElements [nw, ne, sw, se]
      drawBoundries b q = case q of
        Leaf _ -> drawBoundry b
        Node nw ne sw se -> Pictures $ zipWith drawBoundries (QT.divide b ^. partsOf each) [nw, ne, sw, se]
      drawBoundry b@Boundary {center = P (V2 cx cy)} =
        Rect
          (V2 (round $ cx - b.width) (round $ cy + b.height))
          (V2 (round $ cx + b.width) (round $ cy - b.height))
