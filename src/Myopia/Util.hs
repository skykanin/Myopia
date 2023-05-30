-- |
--    Module      : Myopia.Util
--    License     : GNU GPL, version 3 or above
--    Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
--    Stability   : alpha
--    Portability : portable
--  Utility module
module Myopia.Util ((...), uncurry3, showt, withAssetPath, withFontPath) where

import Data.Text (Text)
import Data.Text qualified as Text

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

withAssetPath :: FilePath -> FilePath
withAssetPath = ("./assets/" <>)

withFontPath :: FilePath -> FilePath
withFontPath = ("./assets/fonts/" <>)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

showt :: Show a => a -> Text
showt = Text.pack . show
