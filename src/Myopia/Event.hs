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

import Graphics.SDL (CInt, Point (..), V2 (..))
import Graphics.SDL.Data.Event (Event (eventPayload), EventPayload (KeyboardEvent), InputMotion (..), KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym))
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

updatePosBy :: CInt -> Scancode -> Point V2 CInt -> Point V2 CInt
updatePosBy i ScancodeUp (P (V2 x y)) = P (V2 x (y - i))
updatePosBy i ScancodeDown (P (V2 x y)) = P (V2 x (y + i))
updatePosBy i ScancodeLeft (P (V2 x y)) = P (V2 (x - i) y)
updatePosBy i ScancodeRight (P (V2 x y)) = P (V2 (x + i) y)
updatePosBy _ _ p = p

updatePlayer :: Scancode -> InputMotion -> Player -> Player
updatePlayer keyCode inputMotion player@Player {..} =
  case inputMotion of
    Pressed ->
      player
        { playerMovement = Running
        , position = updatePosBy 5 keyCode position
        }
    Released ->
      player {playerMovement = Idle}

handleEvent :: Event -> GameState -> GameState
handleEvent event gamestate@GameState {..} =
  case eventPayload event of
    KeyboardEvent kbed ->
      let keycode = keysymScancode $ keyboardEventKeysym kbed
          keymotion = keyboardEventKeyMotion kbed
       in gamestate {player = updatePlayer keycode keymotion player}
    _ -> gamestate
