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
import Graphics.SDL.Internal.DrawState (startDrawState)
import Graphics.SDL.Internal.Render (drawPicture)
import SDL (($=))
import SDL.Event (Event, EventPayload (..), eventPayload, pollEvents)
import SDL.Framerate (delay_, with)
import SDL.Video (Renderer, clear, present, rendererDrawColor)
import Prelude hiding (interact)

interact ::
  Renderer -> -- The renderer
  Color -> -- background colour
  world -> -- initial world state
  (world -> Picture) -> -- a function to produce the current picture
  (world -> world) -> -- iterate world
  (Event -> world -> world) -> -- a function to handle input events
  IO ()
interact renderer bgColor world toPicture iterateWorld eventHandler = do
  _ <- with 10 delay_
  clear renderer
  drawPicture renderer startDrawState (toPicture world)
  rendererDrawColor renderer $= bgColor
  events <- pollEvents
  present renderer
  let quit = elem QuitEvent $ map eventPayload events
      newWorld = iterateWorld $ foldr eventHandler world events
  unless quit $ interact renderer bgColor newWorld toPicture iterateWorld eventHandler
