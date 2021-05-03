{-# LANGUAGE RecordWildCards #-}

{- |
   Module      : Graphics.SDL.Internal.Render
   License     : GNU GPL, version 3 or above
   Maintainer  : skykanin <3789764+skykanin@users.noreply.github.com>
   Stability   : alpha
   Portability : portable
 Module for the internal rendering shapes to the screen
-}
module Graphics.SDL.Internal.Render (drawPicture) where

import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.List (find)
import Data.Vector.Storable (fromList)
import Foreign.C.Types (CInt)
import Graphics.SDL.Data.Picture (Name, Picture (..), Position (..), SpriteData (..))
import Graphics.SDL.Internal.DrawState (DrawState (..))
import SDL (Point (..), Rectangle (..), Renderer, Texture, V2 (..), copyEx, rendererRenderTarget, ($=))
import SDL.Image (loadTexture)
import SDL.Primitive

select :: Bool -> a -> a -> a
select b f g = if b then f else g

-- | Draws picture to the render context
drawPicture :: Renderer -> DrawState -> Picture -> IO ()
drawPicture renderer state@DrawState {..} picture =
  case picture of
    Line posA posB -> smoothLine renderer posA posB color
    ThickLine posA posB width -> thickLine renderer posA posB width color
    Triangle posA posB posC ->
      select filled fillTriangle smoothTriangle renderer posA posB posC color
    Rect posA posB ->
      select filled fillRectangle rectangle renderer posA posB color
    Circle pos radius ->
      select filled fillCircle circle renderer pos radius color
    Polygon points ->
      let (xs, ys) = unzip points
       in select filled fillPolygon smoothPolygon renderer (fromList xs) (fromList ys) color
    Fill nextPicture ->
      drawPicture renderer (state {filled = True}) nextPicture
    Color newColor nextPicture ->
      drawPicture renderer (state {color = newColor}) nextPicture
    Sprite name filePath SpriteData {..} -> do
      rendererRenderTarget renderer $= Nothing
      texture <- loadTextureFromCache stateTextures renderer name filePath
      let startPos = getStartPos drawFrom pos size
          rect = Rectangle startPos size
      copyEx renderer texture Nothing (Just rect) rotation rotationPos flipVec
    Pictures pictures ->
      traverse_ (drawPicture renderer state) pictures

-- | Calculate the starting coordinates for drawing the sprite
getStartPos :: Position -> Point V2 CInt -> V2 CInt -> Point V2 CInt
getStartPos drawFrom pos@(P (V2 x y)) (V2 width height) =
  case drawFrom of
    TopLeft -> pos
    BottomLeft -> P (V2 x (y - height))
    Center -> P (V2 (x - (width `div` 2)) (y - (height `div` 2)))

-- | Load new texture unless it already exists in cache
loadTextureFromCache :: IORef [(Name, Texture)] -> Renderer -> Name -> FilePath -> IO Texture
loadTextureFromCache cacheRef renderer texName texPath = do
  textureCache <- readIORef cacheRef
  let inCache = find (\(name, _) -> name == texName) textureCache
  case inCache of
    Nothing -> do
      tex <- loadTexture renderer texPath
      modifyIORef cacheRef ((texName, tex) :)
      pure tex
    Just (_, tex) -> pure tex
