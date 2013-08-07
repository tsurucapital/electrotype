{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.TextBuffer
  ( TextBuffer, new, prepareRender, render, addText, clearText
  , Pen, Vector2(..)
  ) where

import Control.Applicative ((<$>))
import Foreign (FunPtr, Ptr)
import Foreign.C.String (CWString, withCWString)
import Foreign.C.Types (CUInt(..), CInt(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Error (throwIf_)
import Foreign.Storable (Storable(..))
import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup)
import Graphics.Rendering.FreeTypeGL.Internal.Shader (Shader(..))
import Graphics.Rendering.FreeTypeGL.Internal.TextureFont (TextureFont)
import Graphics.Rendering.OpenGL.GL (Vector2(..))

#include "text-buffer.h"

data TextBuffer

foreign import ccall "text_buffer_new"
  c_text_buffer_new :: Shader -> Ptr (Vector2 CInt) -> CInt -> IO (Ptr TextBuffer)

foreign import ccall "text_buffer_prepare_render"
  c_text_buffer_prepare_render :: Ptr TextBuffer -> IO ()

foreign import ccall "text_buffer_render"
  c_text_buffer_render :: Ptr TextBuffer -> IO ()

type Pen = Vector2 Float

foreign import ccall "text_buffer_add_text"
  c_text_buffer_add_text :: Ptr TextBuffer -> Ptr Pen -> Ptr Markup -> Ptr TextureFont -> CWString -> IO CInt

foreign import ccall "text_buffer_clear_text"
  c_text_buffer_clear_text :: Ptr TextBuffer -> IO CInt

foreign import ccall "&text_buffer_delete"
  c_text_buffer_delete :: FunPtr (Ptr TextBuffer -> IO ())

new :: Shader -> Vector2 Int -> Int -> IO (ForeignPtr TextBuffer)
new shader atlasSize depth =
  alloca $ \sizePtr -> do
    poke sizePtr $ fromIntegral <$> atlasSize
    newForeignPtr c_text_buffer_delete =<<
      c_text_buffer_new shader sizePtr (fromIntegral depth)

prepareRender :: ForeignPtr TextBuffer -> IO ()
prepareRender = flip withForeignPtr c_text_buffer_prepare_render

render :: ForeignPtr TextBuffer -> IO ()
render = flip withForeignPtr c_text_buffer_render

addText :: ForeignPtr TextBuffer -> Ptr Markup -> ForeignPtr TextureFont -> ForeignPtr Pen -> String -> IO ()
addText textBuffer markup font pen str =
  throwIf_ (/= 0)
  ((++ "Most likely cause: Out of atlas memory. Try to enlarge the atlas.") .
   (("text_buffer_add_text " ++ show str ++ " returned: ") ++) . show) .
  withCWString str $ \strPtr ->
  withForeignPtr textBuffer $ \textBufferPtr ->
  withForeignPtr font $ \fontPtr ->
  withForeignPtr pen $ \penPtr ->
  c_text_buffer_add_text textBufferPtr penPtr markup fontPtr strPtr

clearText :: ForeignPtr TextBuffer -> IO ()
clearText textBuffer = withForeignPtr textBuffer $ \textBufferPtr ->
    c_text_buffer_clear_text textBufferPtr >> return ()
