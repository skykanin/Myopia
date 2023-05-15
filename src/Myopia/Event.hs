{-# LANGUAGE PatternSynonyms #-}

-- |
--    Module      : Myopia.Event
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Module dealing with game event handling
module Myopia.Event (handleEvent) where

import Myopia.State.Entity (Entity(..), Movement(..))
import Data.Set (Set)
import Data.Set qualified as S
import Graphics.SDL.Data.Event (Event (eventPayload), EventPayload (KeyboardEvent), InputMotion (..), KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym, keyboardEventRepeat))
import Graphics.SDL.Data.Input
  ( Keysym (keysymScancode)
  , Scancode
  , pattern ScancodeA
  , pattern ScancodeD
  , pattern ScancodeS
  , pattern ScancodeW
  )
import Myopia.State.Game (GameState (..))
import Myopia.State.Type (MoveDir (..))
import Optics.Core

scanToDir :: Scancode -> Maybe MoveDir
scanToDir ScancodeW = Just MoveUp
scanToDir ScancodeS = Just MoveDown
scanToDir ScancodeA = Just MoveLeft
scanToDir ScancodeD = Just MoveRight
scanToDir _ = Nothing

updateMoveState :: Set MoveDir -> Movement
updateMoveState moveDirs
  | S.null moveDirs = Idle
  | otherwise = Moving

updateMovements :: Scancode -> InputMotion -> Set MoveDir -> Set MoveDir
updateMovements scancode Pressed = alter S.insert (scanToDir scancode)
updateMovements scancode Released = alter S.delete (scanToDir scancode)

alter :: (a -> Set a -> Set a) -> Maybe a -> Set a -> Set a
alter _ Nothing dirs = dirs
alter f (Just el) dirs = f el dirs

updatePlayer :: Scancode -> InputMotion -> Entity -> Entity
updatePlayer scancode inputMotion player =
  player
    & #moveDirections .~ newMovements
    & #movement .~ updateMoveState newMovements
  where
    newMovements = updateMovements scancode inputMotion player.moveDirections

handleEvent :: Event -> GameState -> GameState
handleEvent event gamestate@GameState {..} =
  case eventPayload event of
    KeyboardEvent kbed ->
      let keycode = keysymScancode $ keyboardEventKeysym kbed
          keymotion = keyboardEventKeyMotion kbed
          repeat = keyboardEventRepeat kbed
      in  if not repeat
            then gamestate {player = updatePlayer keycode keymotion player}
            else gamestate
    _ -> gamestate
