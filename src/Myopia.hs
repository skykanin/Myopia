module Myopia (run) where

import Data.Foldable (traverse_)
import Graphics.SDL
import SDL (Point (..), V2 (..))
import Prelude hiding (interact)

data GameState = GameState
  { fig1 :: [(Name, FilePath)]
  , currentTex1 :: Int
  , fig2 :: [(Name, FilePath)]
  , currentTex2 :: Int
  }

draw :: GameState -> Picture
draw (GameState f1 c1 f2 c2) =
  Pictures [Sprite t1n t1fp sd1, Sprite t2n t2fp sd2]
  where
    (t1n, t1fp) = f1 !! c1
    (t2n, t2fp) = f2 !! c2
    sd1 = scaleBy 5 $ noTransform (P (V2 200 200))
    sd2 = scaleBy 5 $ noTransform (P (V2 400 200))

initState :: [FilePath] -> [FilePath] -> GameState
initState as bs = GameState (withName as) 0 (withName bs) 0

withName :: [FilePath] -> [(Name, FilePath)]
withName = map (\fp -> (take 2 fp, fp))

iterateWorld :: GameState -> GameState
iterateWorld (GameState f1 c1 f2 c2) = GameState f1 nC1 f2 nC2
  where
    nC1 = (c1 + 1) `mod` 4
    nC2 = (c2 + 1) `mod` 4

run :: IO ()
run = interact "Myopia" defaultWindow white startState draw iterateWorld (const id)
  where
    startState = initState ["f0.png", "f1.png", "f2.png", "f3.png"] ["h0.png", "h1.png", "h2.png", "h3.png"]
