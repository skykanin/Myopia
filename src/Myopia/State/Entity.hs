{- |
   Module      : Myopia.State.Entity
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the games entity type
-}
module Myopia.State.Entity
  ( Animation (..)
  , Entity (..)
  , EntityType (..)
  , Movement (..)
  , mkBoundary
  )
where

import Data.Set (Set)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Graphics.SDL (V2 (..))
import Graphics.SDL.Data.Picture (SpriteData (..))
import Myopia.QuadTree (Boundary (..), HasPos (..))
import Myopia.State.Type (Animate (..), MoveDir (..))

-- | Represents any generic entity in the game.
data Entity = Entity
  { animations :: Animation
  , movement :: Movement
  , spriteData :: SpriteData
  , moveDirections :: Set MoveDir
  , entityType :: EntityType
  , id :: UUID
  }
  deriving stock (Generic, Show)

instance Eq Entity where
  e1 == e2 = e1.id == e2.id

instance HasPos Entity Double where
  getPosition entity = fromIntegral <$> entity.spriteData.pos

data Animation = Animation
  { idle :: Animate
  , moving :: Animate
  }
  deriving stock (Generic, Show)

data Movement = Idle | Moving
  deriving stock (Eq, Generic, Show)

data EntityType
  = Player
  | Mob
  deriving stock (Enum, Eq, Generic, Show)

-- | Returns a Boundary from the center point and around the @'Entitiy'@.
mkBoundary :: Entity -> Boundary Double
mkBoundary e = Boundary center width height
  where
    center = fromIntegral <$> e.spriteData.pos
    V2 width height = fromIntegral . (`div` 2) <$> e.spriteData.size
