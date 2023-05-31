{- |
   Module      : Myopia.State.Entity.Mob
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the mob state and related functions
-}
module Myopia.State.Entity.Mob
  ( initMobs
  , iterateMob
  )
where

import Data.Set qualified as S
import Data.UUID.V4 qualified as UUID
import Foreign.C.Types (CInt)
import Graphics.SDL (Point (P), V2 (V2))
import Graphics.SDL.Data.Picture (noTransform, scaleBy)
import Myopia.State.Entity (Animation (..), Entity (..), EntityType (..), Movement (..))
import Myopia.State.Type (Animate (..))
import Myopia.Util qualified as Util
import Optics.Core

idleNames :: [String]
idleNames = map ((<>) "skelet_idle_anim_f" . show) [0 .. 3]

idleSprites :: [FilePath]
idleSprites = (<> ".png") <$> idleNames

initMobs :: IO [Entity]
initMobs = traverse initMob [V2 600 300, V2 100 700, V2 400 200, V2 150 300, V2 200 200]

initMob :: V2 CInt -> IO Entity
initMob pos = do
  uuid <- UUID.nextRandom
  pure $
    Entity
      { animations =
          Animation
            { idle =
                Animate
                  { sprites = zip idleNames (map Util.withAssetPath idleSprites)
                  , currentSprite = 0
                  , slowdown = 6
                  }
            , moving = Animate {sprites = [], currentSprite = 0, slowdown = 6}
            }
      , spriteData = scaleBy 4 $ noTransform (P pos) (V2 16 28)
      , movement = Idle
      , moveDirections = S.empty
      , entityType = Mob
      , id = uuid
      }

iterateMob :: Entity -> Entity
iterateMob mob =
  mob
    & #animations % #idle % #currentSprite
      %~ (\currentSprite -> (currentSprite + 1) `mod` (mob.animations.idle.slowdown * length mob.animations.idle.sprites))
