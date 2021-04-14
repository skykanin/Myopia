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
import Graphics.SDL.Data.Picture (Name, Picture (..), SpriteData (..))
import Graphics.SDL.Internal.DrawState (DrawState (..))
import SDL (Rectangle (..), Renderer, Texture, V2 (..), copyEx, queryTexture, rendererRenderTarget, textureHeight, textureWidth, ($=))
import SDL.Image (loadTexture)
import SDL.Primitive

select :: Bool -> a -> a -> a
select b f g = if b then f else g

-- | Draws picture to the render context
drawPicture :: Renderer -> DrawState -> Picture -> IO ()
drawPicture renderer state@(DrawState {..}) picture =
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
    Sprite name filePath (SpriteData {..}) -> do
      rendererRenderTarget renderer $= Nothing
      texture <- loadTextureFromCache stateTextures renderer name filePath
      texInfo <- queryTexture texture
      let sizeVec = V2 (textureWidth texInfo) (textureHeight texInfo)
          rect = Rectangle pos ((scale *) <$> sizeVec)
      copyEx renderer texture Nothing (Just rect) rotation rotationPos flipVec
    Pictures pictures ->
      traverse_ (drawPicture renderer state) pictures

-- | Load new texture unless it already exists in cache
loadTextureFromCache :: IORef [(Name, Texture)] -> Renderer -> Name -> FilePath -> IO Texture
loadTextureFromCache cacheRef renderer texName texPath = do
  textureCache <- readIORef cacheRef
  let inCache = find (\(name, _) -> name == texName) textureCache
  case inCache of
    Nothing -> do
      tex <- loadTexture renderer texPath
      modifyIORef cacheRef ((texName, tex) :)
      pure $ tex
    Just (_, tex) -> pure tex
