-- |
--    Module      : Graphics.SDL.Internal.Interact
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Module for the internal rendering loop logic
module Graphics.SDL.Internal.Interact (interact) where

import Control.Monad (unless)
import Data.Text (Text)
import Graphics.SDL.Data.Color (Color)
import Graphics.SDL.Data.Picture (Picture)
import Graphics.SDL.Data.Window (WindowConfig)
import Graphics.SDL.Internal.DrawState (DrawState, startDrawState)
import Graphics.SDL.Internal.Render (drawPicture)
import SDL (initializeAll, ($=))
import SDL.Event (Event, EventPayload (..), eventPayload, pollEvents)
import SDL.Framerate (delay_, with)
import SDL.Image (quit)
import SDL.Video (Renderer, clear, createRenderer, createWindow, defaultRenderer, present, rendererDrawColor)
import Prelude hiding (interact)

interact
  :: Text
  -- ^ The window title
  -> WindowConfig
  -- ^ a window configuration
  -> Color
  -- ^ background colour
  -> world
  -- ^ initial world state
  -> (world -> Picture)
  -- ^ a function to produce the current picture
  -> (world -> world)
  -- ^ iterate world
  -> (Event -> world -> world)
  -- ^ a function to handle input events
  -> IO ()
interact winTitle winConf bgColor world toPicture iterateWorld eventHandler = do
  initializeAll
  window <- createWindow winTitle winConf
  renderer <- createRenderer window (-1) defaultRenderer
  drawState <- startDrawState
  -- render state is preserved between frame renders so that textures aren't reloaded
  loop renderer bgColor world drawState toPicture iterateWorld eventHandler
  quit

loop
  :: Renderer
  -> Color
  -> world
  -> DrawState
  -> (world -> Picture)
  -> (world -> world)
  -> (Event -> world -> world)
  -> IO ()
loop renderer bgColor world drawState toPicture iterateWorld eventHandler = do
  _ <- with 60 delay_
  clear renderer
  drawPicture renderer drawState (toPicture world)
  rendererDrawColor renderer $= bgColor
  events <- pollEvents
  present renderer
  let quit = elem QuitEvent $ map eventPayload events
      newWorld = iterateWorld $ foldr eventHandler world events
  unless quit $ loop renderer bgColor newWorld drawState toPicture iterateWorld eventHandler
