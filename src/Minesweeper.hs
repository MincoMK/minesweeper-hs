module Minesweeper where

import SweepEngine
import GraphicsEngine
import Control.Monad (forever, unless)
import Graphics.UI.GLFW
import Graphics.GL
import System.Exit
import GHC.Float (float2Double)
import qualified Data.Vector.Storable as V
import Foreign.Ptr
import Foreign.C (newCString)

startMinesweeper :: Int -> Int -> IO ()
startMinesweeper w h = do
  grid <- makeGrid w h 0.2
  screen <- newScreen 800 800 "Minesweeper"
  let tileWidth = 2 / fromIntegral w
      tileHeight = 2 / fromIntegral h
  let sprites = map (\(x, y) -> Sprite (\_ -> return True) "shaders/vertex.glsl" "shaders/fragment_border.glsl" (Pos (-1 + fromIntegral x * tileWidth + tileWidth /2) (1 - fromIntegral y * tileHeight - tileHeight /2)) (Size tileWidth tileHeight)) [(x, y) | x <- [0..w-1], y <- [0..h-1]]
  shaderedSprites <- initShaders sprites
  
  loop screen shaderedSprites grid w h

loop :: Window -> [ShaderedSprite] ->Grid -> Int -> Int -> IO ()
loop screen shaderedSprites grid w h = do
  shaderedSprites' <- mapM (modifyShaderedSprite $ \sprite -> return sprite { beforeRender = \program -> do
    -- Mouse pointer check logic
    let Pos x y = pos sprite
    let Size width height = spriteSize sprite
    (xPos, yPos) <- getCursorPos screen
    let cXpos = xPos / 400 - 1
    let cYpos = yPos / (-400) + 1
    -- check cXpos and cYpos are inside a sprite.
    let xd = float2Double x
    let yd = float2Double y
    let widthd = float2Double width
    let heightd = float2Double height
    let xInside = cXpos >= xd - widthd / 2 && cXpos <= xd + widthd / 2
    let yInside = cYpos >= yd - heightd / 2 && cYpos <= yd + heightd / 2
    let inside = xInside && yInside
    let color = if inside then V.fromList [0.0 :: Float, 1.0, 0.0, 1.0] else V.fromList [1.0, 0.0, 0.0, 1.0]
    loc <- glGetUniformLocation program =<< newCString "borderColor"
    V.unsafeWith color $ \colorPtr -> do
      glUniform4fv loc 1 $ castPtr colorPtr
    loc1 <- glGetUniformLocation program =<< newCString "borderColor"

    return True
  }) shaderedSprites
  shouldOpen <- render screen shaderedSprites'
  unless shouldOpen exitSuccess
  loop screen shaderedSprites grid w h
