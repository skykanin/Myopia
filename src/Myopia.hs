module Myopia (run) where

import Data.Foldable (traverse_)
import Graphics.SDL
import SDL hiding (trace)
import SDL.Image (loadTexture)
import Prelude hiding (interact)

data GameState = GameState
  { fig1 :: [Texture]
  , currentTex1 :: Int
  , fig2 :: [Texture]
  , currentTex2 :: Int
  }

draw :: GameState -> Picture
draw (GameState f1 c1 f2 c2) =
  Pictures [Sprite t1 sd1, Sprite t2 sd2]
  where
    t1 = f1 !! c1
    t2 = f2 !! c2
    sd1 = scaleBy 5 $ noTransform (P (V2 200 200))
    sd2 = scaleBy 5 $ noTransform (P (V2 400 200))

initState :: [Texture] -> [Texture] -> GameState
initState as bs = GameState as 0 bs 0

iterateWorld :: GameState -> GameState
iterateWorld (GameState f1 c1 f2 c2) = GameState f1 nC1 f2 nC2
  where
    nC1 = (c1 + 1) `mod` 4
    nC2 = (c2 + 1) `mod` 4

run :: IO ()
run = do
  initializeAll
  window <- createWindow "Myopia" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  demonTex <- loadTextures renderer ["f0.png", "f1.png", "f2.png", "f3.png"]
  heroTex <- loadTextures renderer ["h0.png", "h1.png", "h2.png", "h3.png"]
  let startState = initState demonTex heroTex
  interact renderer white startState draw iterateWorld (const id)
  destroyTextures demonTex
  destroyTextures heroTex
  destroyWindow window

destroyTextures :: [Texture] -> IO ()
destroyTextures = traverse_ destroyTexture

loadTextures :: Renderer -> [FilePath] -> IO [Texture]
loadTextures renderer = traverse (loadTexture renderer)
