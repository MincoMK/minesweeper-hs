module Main where

import Minesweeper
import Control.Monad (forever, unless)
import System.Exit (exitSuccess)

main = do
  putStrLn "Minesweeper. Made by MincoMK (@mincomk)"
  startMinesweeper 10 10

