-- |
--    Module      : Myopia.State.Game
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Module defining the overall game state and game state iteration
module Myopia.State.Game
  ( GameState (..)
  , iterateWorld
  , startState
  )
where

import Data.Maybe (listToMaybe, mapMaybe)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Graphics.SDL.Data.Picture (SpriteData (..))
import Myopia.QuadTree
import Myopia.QuadTree qualified as QT
import Myopia.State.Entity (Entity (..))
import Myopia.State.Entity qualified as Entity
import Myopia.State.Entity.Mob (initMobs, iterateMob)
import Myopia.State.Entity.Player (initPlayer, iteratePlayer)
import Myopia.State.Room (Room, startRoom)
import Myopia.State.Tree (initTree)
import Optics.Core

data GameState = GameState
  { player :: Entity
  , mobs :: [Entity]
  , room :: Room
  , quadTree :: QuadTree Double Entity
  }
  deriving stock (Generic, Show)

startState :: IO GameState
startState = do
  player <- initPlayer
  mobs <- initMobs
  pure $
    GameState
      { player = player
      , mobs = mobs
      , room = startRoom
      , quadTree = initTree
      }

iterateWorld :: GameState -> GameState
iterateWorld gameState =
  trace (show $ collide (gameState'.quadTree) enteties') $
    gameState'
      & #quadTree .~ QT.insertElems enteties' initTree
  where
    gameState' =
      gameState
        & #player %~ iteratePlayer
        & #mobs % mapped %~ iterateMob
    enteties' = gameState' ^. #player : gameState' ^. #mobs

collide :: QuadTree Double Entity -> [Entity] -> [String]
collide qt = map (hasCollided . mkSearchBoundary)
  where
    hasCollided :: (Boundary Double, Entity) -> String
    hasCollided (bound, entity) = case checkCollision entity $ QT.elemsInBoundaryExcept entity qt bound of
      Just offending -> unwords ["Entity at", show bound.center, "OVERLAPS WITH", show offending.spriteData.pos]
      Nothing -> "NOTHING COLLIDED !!!"

    -- Make boundary around entity to search for collisions in.
    mkSearchBoundary :: Entity -> (Boundary Double, Entity)
    mkSearchBoundary e = (boundary, e)
      where
        boundary = Boundary center width height
        center = fromIntegral <$> e.spriteData.pos
        V2 width height = (+ 10) . fromIntegral <$> e.spriteData.size

checkCollision :: Entity -> [Entity] -> Maybe Entity
checkCollision e = listToMaybe . mapMaybe (e `overlaps`)
  where
    overlaps :: Entity -> Entity -> Maybe Entity
    overlaps e1 e2
      | QT.doesOverlap (Entity.mkBoundary e1) (Entity.mkBoundary e2) = Just e2
      | otherwise = Nothing
