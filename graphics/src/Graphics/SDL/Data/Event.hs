{- |
   Module      : Graphics.SDL.Data.Event
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 The Event type for dealing with input events
-}
module Graphics.SDL.Data.Event
  ( Event (..)
  , EventPayload (..)
  , InputMotion (..)
  , KeyboardEventData (..)
  )
where

import SDL.Event (Event (..), EventPayload (..), InputMotion (..), KeyboardEventData (..))
