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
import Myopia.Draw (draw)
import Myopia.State.Game (iterateWorld, startState)
import Prelude hiding (interact)

run :: IO ()
run = interact "Myopia" winConf black startState draw iterateWorld (const id)
  where
    winConf = withSize (1200, 800) defaultWindow
