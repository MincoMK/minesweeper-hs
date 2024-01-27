
module GraphicsEngine where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.List              (intercalate, isSuffixOf)
import           Data.Vector.Storable   (Vector)
import qualified Data.Vector.Storable   as V
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL
import           Graphics.UI.GLFW as GLFW
import           Prelude                hiding (init)
import           System.Exit
import Codec.Picture
import           Codec.Picture.Extra    (flipVertically)
import           Codec.Picture.Types
import qualified Codec.Picture.Types    as CPTI
import Data.Word (Word8)

data Sprite = Sprite { beforeRender :: GLuint -> IO Bool, vertexShaderPath :: String, fragmentShaderPath :: String, pos :: Pos, spriteSize :: Size }
data ShaderedSprite = ShaderedSprite { internalSprite :: Sprite, program :: GLuint }
data Pos = Pos { x :: Float, y :: Float }
data Size = Size { width :: Float, height :: Float }
data SizeInt = SizeInt { widthInt :: Int, heightInt :: Int }
data ImageData = ImageData { sizeImage :: SizeInt, dataPtr :: Ptr Word8 }
data TextureData = TextureData { textureId :: GLuint, sizeTexture :: Size }
data ShaderType = Vertex | Fragment

createShader :: String -> ShaderType -> IO GLuint
createShader path typ = do
  shader <- glCreateShader $ case typ of
    Vertex -> GL_VERTEX_SHADER
    Fragment -> GL_FRAGMENT_SHADER
  shaderSource <- readFile path
  compileShader shader shaderSource
  checkCompileErrors shader
  return shader

compileShader :: GLuint -> String -> IO ()
compileShader shader source = do
  shaderSource <- newCString source
  alloca $ \shadersStr -> do
    shadersStr `poke` shaderSource
    glShaderSource shader 1 shadersStr nullPtr
    glCompileShader shader
    checkCompileErrors shader

checkCompileErrors :: GLuint -> IO ()
checkCompileErrors shader = do
  alloca $ \successPtr ->
    alloca $ \infoLogPtr -> do
      glGetShaderiv shader GL_COMPILE_STATUS successPtr
      success <- peek successPtr
      when (success == GL_FALSE) $ do
        glGetShaderInfoLog shader 512 nullPtr infoLogPtr
        infoLog <- peekCString infoLogPtr
        putStrLn infoLog
  return ()

createShaderProgram :: GLuint -> GLuint -> IO GLuint
createShaderProgram vert frag = do
  program <- glCreateProgram
  glAttachShader program vert
  glAttachShader program frag
  glLinkProgram program
  glDeleteShader vert
  glDeleteShader frag
  return program

newScreen :: Int -> Int -> String -> IO Window
newScreen width height title = do
  GLFW.init
  maybeWindow <- GLFW.createWindow width height title Nothing Nothing
  case maybeWindow of
    Nothing -> error "Failed to create GLFW window"
    Just window -> do
      makeContextCurrent maybeWindow
      return window

initShaders :: [Sprite] -> IO [ShaderedSprite]
initShaders = mapM initShader

initShader :: Sprite -> IO ShaderedSprite
initShader sprite = do
  vertexShader <- createShader (vertexShaderPath sprite) Vertex
  fragmentShader <- createShader (fragmentShaderPath sprite) Fragment
  shaderProgram <- createShaderProgram vertexShader fragmentShader
  return $ ShaderedSprite sprite shaderProgram

render :: Window -> [ShaderedSprite] -> IO Bool
render window spriteInternals = do
  shouldClose <- windowShouldClose window
  if shouldClose
    then return False
    else do
      (width, height) <- getWindowSize window
      let aspect = fromIntegral width / fromIntegral height
      renderSprites spriteInternals aspect
      swapBuffers window
      pollEvents
      return True

renderSprites :: [ShaderedSprite] -> Float -> IO ()
renderSprites spriteInternals aspect = do
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT
  renderSprites' spriteInternals aspect

renderSprites' :: [ShaderedSprite] -> Float -> IO ()
renderSprites' [] _ = return ()
renderSprites' (spriteInternal:sprites) aspect = do
  renderSprite spriteInternal aspect
  renderSprites' sprites aspect

modifyShaderedSprite :: (Sprite -> IO Sprite) -> ShaderedSprite -> IO ShaderedSprite
modifyShaderedSprite f spriteInternal = do
  sprite <- f $ internalSprite spriteInternal
  return spriteInternal { internalSprite = sprite }

vertices :: Float -> Float -> Float -> Vector Float
vertices texWidth texHeight' aspect = do
  let texHeight = aspect * texHeight'
  V.fromList
    [
      -texWidth/2, -texHeight/2, 0.0, 0.0,
      texWidth/2, -texHeight/2, 1.0, 0.0,
      texWidth/2, texHeight/2, 1.0, 1.0,
      texWidth/2, texHeight/2, 1.0, 1.0,
      -texWidth/2, texHeight/2, 0.0, 1.0,
      -texWidth/2, -texHeight/2, 0.0, 0.0
      ]

renderSprite :: ShaderedSprite -> Float -> IO ()
renderSprite spriteInternal aspect = do
  let shaderProgram = program spriteInternal
  let sprite = internalSprite spriteInternal

  let position = pos sprite
  let xPos = x position
  let yPos = y position
  
  glUseProgram shaderProgram

  let brFn = beforeRender sprite
  shouldRender <- brFn shaderProgram
  unless shouldRender $ return ()
  
  posLoc <- glGetUniformLocation shaderProgram =<< newCString "pos"
  glUniform2f posLoc xPos yPos

  vaoPtr <- malloc
  vboptr <- malloc

  glGenVertexArrays 1 vaoPtr
  glGenBuffers 1 vboptr

  peek vaoPtr >>= glBindVertexArray
  peek vboptr >>= glBindBuffer GL_ARRAY_BUFFER


  let size = spriteSize sprite
  let verts = vertices (width size) (height size) aspect
  let vertCount = 6
  
  V.unsafeWith verts $ \vertsPtr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral (V.length verts * sizeOf (undefined :: Float))) (castPtr vertsPtr) GL_STATIC_DRAW
  
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (fromIntegral (4 * sizeOf (undefined :: Float))) nullPtr
  glEnableVertexAttribArray 0
  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral $ 4 * sizeOf (undefined :: Float)) $ plusPtr nullPtr (2 * sizeOf (undefined :: Float))
  glEnableVertexAttribArray 1

  glDrawArrays GL_TRIANGLES 0 vertCount
  
  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0


loadTexture :: String -> IO GLuint
loadTexture path = do
  texPtr <- malloc
  glGenTextures 1 texPtr
  glBindTexture GL_TEXTURE_2D =<< peek texPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  bytes <- B.readFile path
  let typ | ".png" `isSuffixOf` path = GL_RGBA | otherwise = GL_RGB
  imageData <- getImageData bytes
  let imageSize = sizeImage imageData
  glTexImage2D GL_TEXTURE_2D 0 (fromIntegral typ)
    (fromIntegral $ widthInt imageSize)
    (fromIntegral $ heightInt imageSize) 0 typ GL_UNSIGNED_BYTE (castPtr $ dataPtr imageData)
  glGenerateMipmap GL_TEXTURE_2D
  tex <- peek texPtr
  free texPtr
  return tex


getImageData :: ByteString -> IO ImageData
getImageData bytes =
  case decodeImage bytes of
    Right (ImageYCbCr8 i) ->
      V.unsafeWith (imageData (convertImage i :: CPTI.Image PixelRGB8)) $ \ptr ->
        return $ ImageData (SizeInt (imageWidth i) (imageHeight i)) ptr
    Right (ImageRGBA8 i) ->
      V.unsafeWith (imageData (flipVertically (convertImage i :: CPTI.Image PixelRGBA8))) $ \ptr ->
        return $ ImageData (SizeInt (imageWidth i) (imageHeight i)) ptr
    _ -> do
      putStrLn "Failed to load texture"
      exitFailure
