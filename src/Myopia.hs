{- |
   Module      : Myopia
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Entry point for the game
-}
module Myopia (run) where

import Graphics.SDL (black, defaultWindow, interact, withSize)
import Myopia.Draw (DebugMode (..), draw)
import Myopia.Event (handleEvent)
import Myopia.State.Game (iterateWorld, startState)
import System.Environment (getArgs)
import Prelude hiding (interact)

run :: IO ()
run = do
  args <- getArgs
  let debugMode = parseDebugMode args
  startState' <- startState
  interact "Myopia" winConf black startState' (draw debugMode) iterateWorld handleEvent
  where
    parseDebugMode xs = case xs of
      "DEBUG" : _ -> Enabled
      _ -> Disabled
    winConf = withSize (1200, 800) defaultWindow
