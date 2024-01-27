module SweepEngine where

import Control.Monad
import System.Random
import GHC.IO (unsafePerformIO)

data Cell = Cell { isMine :: Bool, isFlagged :: Bool, isRevealed :: Bool } deriving (Show)
data Grid = Grid { gridWidth :: Int, gridHeight :: Int, cells :: [Cell] } deriving (Show)

makeGrid :: Int -> Int -> Float -> IO Grid
makeGrid w h mineChance = do
  gen <- newStdGen
  let mines = map (< mineChance) (randoms gen :: [Float])
  return $ Grid w h $ map (\x -> Cell (mines !! x) False False) [0..w*h-1]

getCellAt :: Grid -> Int -> Int -> Cell
getCellAt grid x y = cells grid !! (y * gridWidth grid + x)
