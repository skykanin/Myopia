{- |
   Module      : Graphics.SDL.Internal.Interact
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module for the internal rendering loop logic
-}
module Graphics.SDL.Internal.Interact (interact) where

import Control.Monad (unless)
import Graphics.SDL.Data.Color (Color)
import Graphics.SDL.Data.Picture (Picture)
import Graphics.SDL.Data.Window (WindowConfig, WindowTitle)
import Graphics.SDL.Internal.DrawState (startDrawState)
import Graphics.SDL.Internal.Render (drawPicture)
import SDL (initializeAll, ($=))
import SDL.Event (Event, EventPayload (..), eventPayload, pollEvents)
import SDL.Framerate (delay_, with)
import SDL.Image (quit)
import SDL.Video (Renderer, clear, createRenderer, createWindow, defaultRenderer, present, rendererDrawColor)
import Prelude hiding (interact)

interact ::
  -- | The window title
  WindowTitle ->
  -- | a window configuration
  WindowConfig ->
  -- | background colour
  Color ->
  -- | initial world state
  world ->
  -- | a function to produce the current picture
  (world -> Picture) ->
  -- | iterate world
  (world -> world) ->
  -- | a function to handle input events
  (Event -> world -> world) ->
  IO ()
interact winTitle winConf bgColor world toPicture iterateWorld eventHandler = do
  initializeAll
  window <- createWindow winTitle winConf
  renderer <- createRenderer window (-1) defaultRenderer
  loop renderer bgColor world toPicture iterateWorld eventHandler
  quit

loop ::
  Renderer ->
  Color ->
  world ->
  (world -> Picture) ->
  (world -> world) ->
  (Event -> world -> world) ->
  IO ()
loop renderer bgColor world toPicture iterateWorld eventHandler = do
  _ <- with 10 delay_
  clear renderer
  drawState <- startDrawState
  drawPicture renderer drawState (toPicture world)
  rendererDrawColor renderer $= bgColor
  events <- pollEvents
  present renderer
  let quit = elem QuitEvent $ map eventPayload events
      newWorld = iterateWorld $ foldr eventHandler world events
  unless quit $ loop renderer bgColor newWorld toPicture iterateWorld eventHandler
