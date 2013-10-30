{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}
module Graphics.Rendering.Electrotype.Internal.VertexBuffer
( VertexBuffer
, newVertexBuffer, destroyVertexBuffer
, clearVertexBuffer, insertString, insertByteString
, renderVertexBuffer
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B (unsafeUseAsCStringLen)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Linear

import Graphics.Rendering.Electrotype.Internal.TextureFont
import Graphics.Rendering.OpenGL.Raw.Core31 (GLenum, gl_TRIANGLES)
import Graphics.Rendering.OpenGL.GL.BeginEnd

#include "vertex-buffer.h"

data VertexBufferRef
newtype VertexBuffer = VertexBuffer (ForeignPtr VertexBufferRef)

foreign import ccall unsafe "vertex_buffer_new"
    c_vertex_buffer_new :: CString -> IO (Ptr VertexBufferRef)
foreign import ccall unsafe "vertex_buffer_delete"
    c_vertex_buffer_delete :: Ptr VertexBufferRef -> IO ()
foreign import ccall unsafe "vertex_buffer_clear"
    c_vertex_buffer_clear :: Ptr VertexBufferRef -> IO ()

newVertexBuffer :: IO VertexBuffer
newVertexBuffer = withCString "vertex:3f,tex_coord:2f,color:4f" $ \cstr -> do
    ref <- newForeignPtr_ =<< c_vertex_buffer_new cstr
    return (VertexBuffer ref)

destroyVertexBuffer :: VertexBuffer -> IO ()
destroyVertexBuffer (VertexBuffer ref) =
    withForeignPtr ref c_vertex_buffer_delete

clearVertexBuffer :: VertexBuffer -> IO ()
clearVertexBuffer (VertexBuffer ref) =
    withForeignPtr ref c_vertex_buffer_clear

foreign import ccall unsafe "vertex_buffer_add_text"
    c_vertex_buffer_add_text
        :: Ptr VertexBufferRef -> Ptr TextureFontRef -> CWString
        -> Ptr (V4 Float) -> Ptr (V2 Float) -> IO ()

foreign import ccall unsafe "vertex_buffer_add_char8_len"
    c_vertex_buffer_add_char8_len
        :: Ptr VertexBufferRef -> Ptr TextureFontRef -> Ptr CChar -> CSize
        -> Ptr (V4 Float) -> Ptr (V2 Float) -> IO ()

insertString
    :: VertexBuffer
    -- ^ Vertex buffer to insert text into.
    -> TextureFont
    -- ^ Font to use for the inserted text.
    -> String
    -- ^ The string to insert.
    -> V4 Float
    -- ^ RGBA color for the text to be inserted.
    -> V2 Float
    -- ^ Beginning position for inserting the text.
    -> IO (V2 Float)
    -- ^ Position you would use to append more text to the line.
insertString (VertexBuffer vertexRef) (TextureFont _ fontRef) str color pos =
    withForeignPtr vertexRef $ \vertexPtr ->
    withForeignPtr fontRef $ \fontPtr ->
    with color $ \colorPtr ->
    with pos $ \posPtr ->
    withCWString str $ \cwstr -> do
    c_vertex_buffer_add_text vertexPtr fontPtr cwstr colorPtr posPtr 
    peek posPtr

insertByteString
    :: VertexBuffer
    -- ^ Vertex buffer to insert text into.
    -> TextureFont
    -- ^ Font to use for the inserted text.
    -> B.ByteString
    -- ^ The string to insert.
    -> V4 Float
    -- ^ RGBA color for the text to be inserted.
    -> V2 Float
    -- ^ Beginning position for inserting the text.
    -> IO (V2 Float)
    -- ^ Position you would use to append more text to the line.
insertByteString (VertexBuffer vertexRef) (TextureFont _ fontRef) str color pos =
    withForeignPtr vertexRef $ \vertexPtr ->
    withForeignPtr fontRef $ \fontPtr ->
    with color $ \colorPtr ->
    with pos $ \posPtr ->
    B.unsafeUseAsCStringLen str $ \(charPtr, len) -> do
    c_vertex_buffer_add_char8_len vertexPtr fontPtr charPtr (fromIntegral len) colorPtr posPtr 
    peek posPtr

foreign import ccall unsafe "vertex_buffer_render"
    c_vertex_buffer_render :: Ptr VertexBufferRef -> GLenum -> IO ()

renderVertexBuffer :: VertexBuffer -> IO ()
renderVertexBuffer (VertexBuffer ref) = withForeignPtr ref $ \ptr ->
    c_vertex_buffer_render ptr gl_TRIANGLES
