{- |
   Module      : Myopia.State.Game
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module defining the overall game state and game state iteration
-}
module Myopia.State.Game (
  GameState (..),
  iterateWorld,
  startState,
) where

import Myopia.State.Player (Player (..), initPlayer, iteratePlayer)
import Myopia.State.Room (Room, startRoom)

data GameState = GameState
  { player :: Player
  , room :: Room
  }
  deriving (Show)

startState :: GameState
startState =
  GameState
    { player = initPlayer
    , room = startRoom
    }

iterateWorld :: GameState -> GameState
iterateWorld gs@GameState {..} = gs {player = iteratePlayer player}
