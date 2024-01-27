module FontEngine where

import FreeType.Core.Base
import FreeType.Core.Types
import Graphics.GL
import Foreign.Ptr
import Foreign.Storable

type FontLibrary = FT_Library
type FontFace = FT_Face

initFontEngine :: IO FontLibrary
initFontEngine = ft_Init_FreeType

createFace :: FontLibrary -> FilePath -> IO FontFace
createFace lib path = ft_New_Face lib path 0

freeFace :: FontFace -> IO ()
freeFace = ft_Done_Face

loadChar :: FontFace -> Char -> IO ()
loadChar face char = ft_Load_Char face (fromIntegral $ fromEnum char) 0

loadCharGL :: FontFace -> Char -> IO ()
loadCharGL face char = do
  loadChar face char
  glyphSlot <- peek . frGlyph =<< peek face
  let bitmap = gsrBitmap glyphSlot
  glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral $ bWidth bitmap) (fromIntegral $ bRows bitmap) 0 GL_RED GL_UNSIGNED_BYTE $ castPtr $ bBuffer bitmap
