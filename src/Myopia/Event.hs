{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

{- |
   Module      : Myopia.Event
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module dealing with game event handling
-}
module Myopia.Event (handleEvent) where

import Graphics.SDL.Data.Event (Event (eventPayload), EventPayload (KeyboardEvent), InputMotion (..), KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym, keyboardEventRepeat))
import Graphics.SDL.Data.Input (
  Keysym (keysymScancode),
  Scancode,
  pattern ScancodeDown,
  pattern ScancodeLeft,
  pattern ScancodeRight,
  pattern ScancodeUp,
 )
import Myopia.State.Game (GameState (..))
import Myopia.State.Player (Player (..), PlayerMovement (..))
import Myopia.State.Type (MoveDir (..))
import Prelude hiding (Left, Right)

updateMovement :: Scancode -> MoveDir -> MoveDir
updateMovement ScancodeUp _ = Up
updateMovement ScancodeDown _ = Down
updateMovement ScancodeLeft _ = Left
updateMovement ScancodeRight _ = Right
updateMovement _ d = d

updateMoveState :: Scancode -> PlayerMovement
updateMoveState scancode = if scancode `elem` [ScancodeUp, ScancodeDown, ScancodeLeft, ScancodeRight] then Running else Idle

updatePlayer :: Scancode -> InputMotion -> Player -> Player
updatePlayer scancode inputMotion player@Player {..} =
  case inputMotion of
    Pressed ->
      player
        { moveDirection = updateMovement scancode moveDirection
        , playerMovement = updateMoveState scancode
        }
    Released ->
      player {playerMovement = Idle}

handleEvent :: Event -> GameState -> GameState
handleEvent event gamestate@GameState {..} =
  case eventPayload event of
    KeyboardEvent kbed ->
      let keycode = keysymScancode $ keyboardEventKeysym kbed
          keymotion = keyboardEventKeyMotion kbed
          repeat = keyboardEventRepeat kbed
       in if not repeat
            then gamestate {player = updatePlayer keycode keymotion player}
            else gamestate
    _ -> gamestate
