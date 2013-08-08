{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, RecordWildCards, NamedFieldPuns #-}
module Graphics.Rendering.FreeTypeGL.Internal.TextureFont
( TextureAtlasRef, TextureAtlas(..), newTextureAtlas
, getTextureAtlasId
, TextureFontRef, TextureFont(..), newTextureFont
, IsLCD(..), textureFontLoadGlyphsString
, Vector2(..)
) where

--import qualified Data.Text as T
--import qualified Data.Text.Foreign as T
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Unsafe as B
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Graphics.Rendering.OpenGL.GL (Vector2(..))

#include "texture-atlas.h"

-- Texture atlas

data TextureAtlasRef

newtype TextureAtlas = TextureAtlas (ForeignPtr TextureAtlasRef)
    deriving (Eq, Ord, Show)

foreign import ccall unsafe "texture_atlas_new"
    c_texture_atlas_new :: CSize -> CSize -> CSize -> IO (Ptr TextureAtlasRef)
foreign import ccall unsafe "&texture_atlas_delete"
    c_texture_atlas_delete :: FunPtr (Ptr TextureAtlasRef -> IO ())

newTextureAtlas
    :: Int -> Int -> Int
    -> IO TextureAtlas
newTextureAtlas width height depth = do
    ref <- newForeignPtr c_texture_atlas_delete =<< c_texture_atlas_new
        (fromIntegral width) (fromIntegral height) (fromIntegral depth)
    return (TextureAtlas ref)

newtype TextureAtlasId = TextureAtlasId CUInt

instance Storable TextureAtlasId where
    sizeOf _ = (#size texture_atlas_t)
    alignment _ = alignment (undefined :: CSize)
    peek ptr = do
        n <- (#peek texture_atlas_t, id) ptr
        return (TextureAtlasId n)
    poke ptr (TextureAtlasId n) = do
        (#poke texture_atlas_t, id) ptr n

getTextureAtlasId :: TextureAtlas -> IO CUInt
getTextureAtlasId (TextureAtlas atlas) = withForeignPtr atlas $ \ptr -> do
    TextureAtlasId n <- peek (castPtr ptr)
    return n

    {-
data TextureAtlas = TextureAtlas
    { taNodes :: Ptr ()
    , taWidth :: CSize
    , taHeight :: CSize
    , taDepth :: CSize
    , taUsed :: CSize
    , taId :: CUInt
    , taData :: Ptr ()
    }

instance Storable TextureAtlas where
    sizeOf _ = (#size texture_atlas_t)
    alignment _ = alignment (undefined :: CSize)
    peek ptr = do
        taNodes <- (#peek texture_atlas_t, 
        -}

data TextureFontRef

data TextureFont = TextureFont TextureAtlas (ForeignPtr TextureFontRef)
    deriving (Eq, Ord, Show)

foreign import ccall unsafe "texture_font_new"
  c_texture_font_new :: Ptr TextureAtlasRef -> CString -> CFloat -> IO (Ptr TextureFontRef)
foreign import ccall unsafe "&texture_font_delete"
  c_texture_font_delete :: FunPtr (Ptr TextureFontRef -> IO ())

-- TODO wtf? use newCString
foreign import ccall unsafe "strdup"
  c_strdup :: CString -> IO CString

data IsLCD = IsLCD | NotLCD
  deriving (Read, Show, Eq, Ord)

newTextureFont
    :: TextureAtlas -> FilePath -> Float
    -> IO TextureFont
newTextureFont (TextureAtlas atlas) filename size =
    withForeignPtr atlas $ \atlasPtr ->
    withCString filename $ \filenamePtr -> do
    newFilenamePtr <- c_strdup filenamePtr
    opaque <- newForeignPtr c_texture_font_delete =<<
    -- TODO no throwing!
        throwIfNull ("Failed to make texture font for " ++ show filename)
        (c_texture_font_new atlasPtr newFilenamePtr (realToFrac size))
    return (TextureFont (TextureAtlas atlas) opaque)
    {-
    where
    toBool IsLCD = True
    toBool NotLCD = False
    -}

foreign import ccall unsafe "texture_font_load_glyphs"
    c_texture_font_load_glyphs :: Ptr TextureFontRef -> CWString -> IO CSize

textureFontLoadGlyphsString :: TextureFont -> String -> IO Int
textureFontLoadGlyphsString (TextureFont _ ref) str =
    withCWString str $ \cwstr ->
    withForeignPtr ref $ \ptr ->
    fromIntegral `fmap` c_texture_font_load_glyphs ptr cwstr

{-
textureFontLoadGlyphsText :: TextureFont -> T.Text -> IO ()
textureFontLoadGlyphsText (TextureFont ref) txt =
    useAsPtr
-}
--textureFont
