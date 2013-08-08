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
( module Graphics.Rendering.FreeTypeGL.Internal.Shader
, module Graphics.Rendering.FreeTypeGL.Internal.VertexBuffer
, module Graphics.Rendering.FreeTypeGL.Internal.TextureFont
) where

import Graphics.Rendering.FreeTypeGL.Internal.Shader
import Graphics.Rendering.FreeTypeGL.Internal.TextureFont
import Graphics.Rendering.FreeTypeGL.Internal.VertexBuffer
