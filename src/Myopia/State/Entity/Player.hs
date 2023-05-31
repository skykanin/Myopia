{- |
   Module      : Myopia.State.Entity.Player
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the player state and related functions
-}
module Myopia.State.Entity.Player
  ( initPlayer
  , iteratePlayer
  )
where

import Data.Set (Set)
import Data.Set qualified as S
import Data.UUID.V4 qualified as UUID
import Graphics.SDL (CInt, Point (P), V2 (V2))
import Graphics.SDL.Data.Picture (SpriteData (..), noTransform, scaleBy)
import Myopia.State.Entity
import Myopia.State.Type (Animate (..), MoveDir (..))
import Myopia.Util qualified as Util
import Optics.Core

idleNames :: [String]
idleNames = map ((<>) "wizzard_f_idle_f" . show) [0 .. 3]

idleSprites :: [FilePath]
idleSprites = map (\n -> concat ["wizzard_f_idle_anim_f", show n, ".png"]) [0 .. 3]

runNames :: [String]
runNames = map ((<>) "wizzard_f_run_f" . show) [0 .. 3]

runSprites :: [FilePath]
runSprites = map (\n -> concat ["wizzard_f_run_anim_f", show n, ".png"]) [0 .. 3]

initPlayer :: IO Entity
initPlayer = do
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
            , moving =
                Animate
                  { sprites = zip runNames (map Util.withAssetPath runSprites)
                  , currentSprite = 0
                  , slowdown = 6
                  }
            }
      , movement = Idle
      , moveDirections = S.empty
      , spriteData = scaleBy 4 $ noTransform (P (V2 600 400)) (V2 16 28)
      , entityType = Player
      , id = uuid
      }

movePlayerBy :: CInt -> MoveDir -> Point V2 CInt -> Point V2 CInt
movePlayerBy i mv (P (V2 x y)) = P $ case mv of
  MoveUp -> V2 x (y - i)
  MoveDown -> V2 x (y + i)
  MoveLeft -> V2 (x - i) y
  MoveRight -> V2 (x + i) y

-- | Change sprite flip state based on movement direction
flipSprite :: Set MoveDir -> V2 Bool -> V2 Bool
flipSprite moveDirs flipSprite
  | MoveLeft `S.member` moveDirs = V2 True False
  | MoveRight `S.member` moveDirs = V2 False False
  | otherwise = flipSprite

-- | Clamp movement speed when moving diagonally
clampMoveLength :: CInt -> Set MoveDir -> CInt
clampMoveLength moveBy moveDirs
  | S.size moveDirs >= 2 = round $ fromIntegral @_ @Float moveBy / sqrt 2
  | otherwise = moveBy

iteratePlayer :: Entity -> Entity
iteratePlayer player =
  case player.movement of
    Idle ->
      player
        & #animations % #idle % #currentSprite
          %~ (\currentSprite -> (currentSprite + 1) `mod` (player.animations.idle.slowdown * length player.animations.idle.sprites))
    Moving ->
      player
        & #animations % #moving % #currentSprite
          %~ ( \currentSprite -> (currentSprite + 1) `mod` (player.animations.moving.slowdown * length player.animations.moving.sprites)
             )
        & #spriteData % #pos
          %~ ( \pos ->
                let moveSpeed = clampMoveLength 6 player.moveDirections
                in  foldr (movePlayerBy moveSpeed) pos player.moveDirections
             )
        & #spriteData % #flipVec %~ flipSprite player.moveDirections
