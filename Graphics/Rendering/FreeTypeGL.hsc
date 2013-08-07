{-# LANGUAGE ForeignFunctionInterface #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeTypeGL
-- Copyright   :  (C) 2012 Eyal Lotem
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Eyal Lotem <eyal.lotem@gmail.com>
-- Stability   :  experimental
-- Portability :  Haskell2010
--
-- FreeTypeGL lets one load and use texture fonts in an OpenGL
-- context.  You can use it with GLFW-b
-- (<http://hackage.haskell.org/package/GLFW-b>), GLUT
-- (<http://hackage.haskell.org/package/GLUT>), or any other GL context
-- you wish.  The main benefit of this library is that it can be
-- installed with a pure "cabal install", no need to manually install
-- any C bits.
-------------------------------------------------------------------------------

module Graphics.Rendering.FreeTypeGL
  ( FontDesc(..), fontDescFindFileName
  , Shader, newShader
  , Font(fFont), loadFont, textRendererSize
  , Markup(..), noMarkup
  , TextRenderer(..), textRenderer
  , prepareRenderText, renderText
  , setText, appendText, setByteString, appendByteString
  , setRendererContents
  , Vector2(..), Color4(..)
  ) where

import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca, malloc, finalizerFree)
import Foreign.Marshal.Error (throwIf_)
import Foreign.Storable (peek, poke)
import Graphics.Rendering.FreeTypeGL.Internal.Markup
import Graphics.Rendering.FreeTypeGL.Internal.Shader (Shader)
import Graphics.Rendering.OpenGL.GL (Color4(..), Vector2(..))
import Paths_FreeTypeGL (getDataFileName)
import qualified Graphics.Rendering.FreeTypeGL.Internal.FontDesc as IFD
import qualified Graphics.Rendering.FreeTypeGL.Internal.Shader as Shader
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextBuffer as ITB
import Graphics.Rendering.FreeTypeGL.Internal.Font

-- FontDesc:

-- | A 'FontDesc' describes the desired properties of a system font.
-- It's only purpose is querying the font_config library for the
-- desired font.
--
-- NOTE: This functionality is only available if the
-- package was configured with the use_font_config flag (e.g: cabal
-- install -f use_font_config).
data FontDesc = FontDesc
  { fdFamily :: String
  , fdSize :: Float
  , fdBold :: Bool
  , fdItalic :: Bool
  }

-- | Look up a system font file.
--
-- NOTE: This functionality is only available if the package was
-- configured with the use_font_config flag
-- (e.g: @cabal install -f use_font_config@).
fontDescFindFileName :: FontDesc -> IO FilePath
fontDescFindFileName (FontDesc family size bold italic) =
  withCString family $ \familyPtr ->
  IFD.fontDescFindFileName $ IFD.FontDesc familyPtr size bold italic

-- Shader:

#include "shader.h"

foreign import ccall "freetypegl_init"
  c_freetypegl_init :: IO CInt


initialize :: IO ()
initialize = throwIf_ (/= 0) (("freetypegl_init returned" ++) . show) c_freetypegl_init

-- | Make a 'Shader' needed for 'loadFont'.
newShader :: IO Shader
newShader = do
  initialize
  textVert <- getDataFileName "shaders/text.vert"
  textFrag <- getDataFileName "shaders/text.frag"
  Shader.load textVert textFrag

-- | A 'TextRenderer' is an intermediate representation of all font
-- renderings. It represents the GL program to render the font, and
-- may be re-used multiple times.  Re-using a TextRenderer is faster
-- than computing the same renderer multiple times.
data TextRenderer = TextRenderer
  { trBuffer :: ForeignPtr ITB.TextBuffer
  , trPen :: ForeignPtr ITB.Pen
  }

-- | Make a 'TextRenderer' for a given font.
textRenderer :: Shader -> IO TextRenderer
textRenderer shader = do
  textBuffer <- ITB.new shader (Vector2 512 512) 1
  penPtr <- malloc
  poke penPtr (Vector2 0 0)
  pen <- newForeignPtr finalizerFree penPtr
  return $ TextRenderer textBuffer pen

-- | Append text to the end of a TextRenderer.
appendText :: TextRenderer -> Font -> Markup -> String -> IO ()
appendText (TextRenderer textBuffer pen) (Font font) markup str =
  alloca $ \markupPtr -> do
  poke markupPtr markup
  ITB.addText textBuffer markupPtr font pen str

appendByteString :: TextRenderer -> Font -> Markup -> B.ByteString -> IO ()
appendByteString (TextRenderer textBuffer pen) (Font font) markup str = do
  alloca $ \markupPtr -> do
  poke markupPtr markup
  ITB.addByteString textBuffer markupPtr font pen str

-- | Replace all existing text in a TextRenderer.
setText :: TextRenderer -> Font -> Markup -> String -> IO ()
setText tr font markup str = do
  ITB.clearText (trBuffer tr)
  withForeignPtr (trPen tr) $ \pen ->
    poke pen (Vector2 0 0)
  appendText tr font markup str

setByteString :: TextRenderer -> Font -> Markup -> B.ByteString -> IO ()
setByteString tr font markup str = do
  ITB.clearText (trBuffer tr)
  withForeignPtr (trPen tr) $ \pen ->
    poke pen (Vector2 0 0)
  appendByteString tr font markup str

prepareRenderText :: TextRenderer -> Font -> IO ()
prepareRenderText tr font = do
  -- One final empty append, to force any pending recalculation of line height.
  -- This could be done more elegantly.
  appendText tr font noMarkup ""
  ITB.prepareRender (trBuffer tr)

-- | Render a 'TextRenderer' to the GL context
--
-- NOTE: This will have the following side effects:
--
-- * GL's blend will be enabled
--
-- * GL's Texture2D will be enabled
--
-- * GL's color will be changed
--
-- * GL's color_material will be disabled
--
-- * GL's blend_func will be set to (SRC_ALPHA, ONE_MINUS_SRC_ALPHA)
--
-- * GL's blend_color will be modified
renderText :: TextRenderer -> IO ()
renderText = ITB.render . trBuffer

-- | Get the size of a 'TextRenderer'
--
-- NOTE: If you don't need to also render the text, it is better to
-- use the faster 'textSize' function.
textRendererSize :: TextRenderer -> IO (Vector2 Float)
textRendererSize tr = withForeignPtr (trPen tr) peek

renderSpans :: Font -> Markup -> Maybe String -> [MarkSpan] -> [TextRenderer -> IO ()]
renderSpans _ _ Nothing [] = []
renderSpans font markup (Just str) [] = [\tr -> appendText tr font markup str]
renderSpans font markup Nothing (SpanStyle ms : rest) = renderSpans font (addStyle ms markup) Nothing rest
renderSpans font markup Nothing (SpanString str : rest) = renderSpans font markup (Just str) rest
renderSpans font markup (Just str) (SpanStyle ms : rest) =
    [\tr -> appendText tr font markup str] ++ renderSpans font (addStyle ms markup) Nothing rest
renderSpans oldFont markup (Just str) (SpanFont font : rest) =
    [\tr -> appendText tr oldFont markup str] ++ renderSpans font markup Nothing rest
renderSpans _    markup Nothing (SpanFont font : rest) =
    renderSpans font markup Nothing rest
renderSpans font markup (Just str1) (SpanString str2 : rest) =
    renderSpans font markup (Just (str1 `mappend` str2)) rest

setRendererContents :: TextRenderer -> Font -> Marked -> IO ()
setRendererContents renderer font (Marked marks) =
     sequence_ $ map ($ renderer) (renderSpans font noMarkup Nothing marks)
