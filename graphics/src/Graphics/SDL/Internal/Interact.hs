-- |
--    Module      : Graphics.SDL.Internal.Interact
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Module for the internal rendering loop logic
module Graphics.SDL.Internal.Interact (interact) where

import Data.Map.Strict (Map)
import Data.Foldable (traverse_)
import Data.IORef (IORef, readIORef)
import Control.Monad (unless)
import Data.Text (Text)
import Graphics.SDL.Data.Color (Color)
import Graphics.SDL.Data.Picture (Picture)
import Graphics.SDL.Data.Window (WindowConfig)
import Graphics.SDL.Internal.DrawState (DrawState(..), startDrawState)
import Graphics.SDL.Internal.Render (drawPicture)
import SDL (initializeAll, ($=))
import SDL.Event (Event, EventPayload (..), eventPayload, pollEvents)
import SDL.Framerate (delay_, with)
import SDL.Image qualified as Image
import SDL.Font (Font)
import SDL.Font qualified as Font
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
  drawState <- startDrawState
  withSDLResources drawState.fonts $ do
    window <- createWindow winTitle winConf
    renderer <- createRenderer window (-1) defaultRenderer
    -- render state is preserved between frame renders so that textures aren't reloaded
    loop renderer bgColor world drawState toPicture iterateWorld eventHandler

-- | Run a computation inside an SDL resource handler
withSDLResources :: IORef (Map FilePath Font) -> IO () -> IO ()
withSDLResources fontRef io = do
  initializeAll
  Font.initialize
  io
  fonts <- readIORef fontRef
  traverse_ Font.free fonts
  Font.quit
  Image.quit

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
  let hasQuit = elem QuitEvent $ map eventPayload events
      newWorld = iterateWorld $ foldr eventHandler world events
  unless hasQuit $ loop renderer bgColor newWorld drawState toPicture iterateWorld eventHandler
