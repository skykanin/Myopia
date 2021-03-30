module Myopia (run) where

import Control.Monad (unless)
import SDL

run :: IO ()
run = do
  initializeAll
  window <- createWindow "Myopia" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 200 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect renderer (Just (Rectangle (P (V2 0 0)) (V2 400 200)))
  present renderer
  unless qPressed (appLoop renderer)
