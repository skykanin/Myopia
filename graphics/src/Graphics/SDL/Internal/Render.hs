{- |
   Module      : Graphics.SDL.Internal.Render
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module for the internal rendering shapes to the screen
-}
module Graphics.SDL.Internal.Render (drawPicture) where

import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Storable (fromList)
import Foreign.C.Types (CInt)
import Graphics.SDL.Data.Picture (Picture (..), Position (..), SpriteData (..))
import Graphics.SDL.Internal.DrawState (DrawState (..))
import SDL (Point (..), Rectangle (..), Renderer, Texture, V2 (..), copyEx, rendererRenderTarget, ($=))
import SDL.Font (Font, blended, load, setStyle)
import SDL.Image (loadTexture)
import SDL.Primitive
import SDL.Video.Renderer (createTextureFromSurface, freeSurface)

select :: Bool -> a -> a -> a
select b f g = if b then f else g

-- | Draws picture to the render context
drawPicture :: Renderer -> DrawState -> Picture -> IO ()
drawPicture renderer state picture =
  case picture of
    Line posA posB -> smoothLine renderer posA posB state.color
    ThickLine posA posB width -> thickLine renderer posA posB width state.color
    Triangle posA posB posC ->
      select state.filled fillTriangle smoothTriangle renderer posA posB posC state.color
    Rect posA posB ->
      select state.filled fillRectangle rectangle renderer posA posB state.color
    Circle pos radius ->
      select state.filled fillCircle circle renderer pos radius state.color
    Polygon points ->
      let (xs, ys) = unzip points
      in  select state.filled fillPolygon smoothPolygon renderer (fromList xs) (fromList ys) state.color
    Fill nextPicture ->
      drawPicture renderer (state {filled = True}) nextPicture
    Color newColor nextPicture ->
      drawPicture renderer (state {color = newColor}) nextPicture
    Sprite name filePath SpriteData {..} -> do
      rendererRenderTarget renderer $= Nothing
      texture <- loadTextureFromCache state.textures renderer name filePath
      let startPos = getStartPos drawFrom pos size
          rect = Rectangle startPos size
      copyEx renderer texture Nothing (Just rect) rotation rotationPos flipVec
    Text fontPath fontSize style rectSize text position -> do
      font <- loadFontFromCache state.fonts fontPath fontSize
      for_ style $ setStyle font . pure
      surface <- blended font state.color text
      texture <- createTextureFromSurface renderer surface
      let rect = Rectangle position rectSize
      copyEx renderer texture Nothing (Just rect) 0 Nothing (V2 False False)
      -- free intermediary surface
      freeSurface surface
    Pictures pictures ->
      traverse_ (drawPicture renderer state) pictures

loadFontFromCache :: IORef (Map String Font) -> FilePath -> CInt -> IO Font
loadFontFromCache cacheRef fontPath size = do
  fontCache <- readIORef cacheRef
  case Map.lookup fontPath fontCache of
    Nothing -> do
      font <- load fontPath $ fromIntegral size
      modifyIORef cacheRef (Map.insert fontPath font)
      pure font
    Just font -> pure font

-- | Calculate the starting coordinates for drawing the sprite
getStartPos :: Position -> Point V2 CInt -> V2 CInt -> Point V2 CInt
getStartPos drawFrom pos@(P (V2 x y)) (V2 width height) =
  case drawFrom of
    TopLeft -> pos
    BottomLeft -> P $ V2 x (y - height)
    -- TODO: width and height will be rounded in case of odd values which isn't ideal
    Center -> P $ V2 (x - (width `div` 2)) (y - (height `div` 2))

-- | Load new texture unless it already exists in cache
loadTextureFromCache :: IORef [(String, Texture)] -> Renderer -> String -> FilePath -> IO Texture
loadTextureFromCache cacheRef renderer texName texPath = do
  textureCache <- readIORef cacheRef
  let inCache = find (\(name, _) -> name == texName) textureCache
  case inCache of
    Nothing -> do
      tex <- loadTexture renderer texPath
      modifyIORef cacheRef ((texName, tex) :)
      pure tex
    Just (_, tex) -> pure tex
