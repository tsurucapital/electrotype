{-# LANGUAGE CPP #-}
module Graphics.Rendering.Electrotype.Baked.Shaders
( withTempShaderPaths, v3ft2fc4fVert, v3ft2fc4fFrag
, simple2dVert, simple2dFrag
) where

import System.FilePath ((</>))
import System.IO.Temp

withTempShaderPaths :: (FilePath -> (FilePath, FilePath) -> IO a) -> IO a
withTempShaderPaths f = withSystemTempDirectory "electrotype_shaders" $ \tmpPath -> do
    let vertPath = (tmpPath </> "v3f-2f-c4f.vert")
        fragPath = (tmpPath </> "v3f-2f-c4f.frag")
    writeFile vertPath v3ft2fc4fVert
    writeFile fragPath v3ft2fc4fFrag
    f tmpPath (vertPath, fragPath)

v3ft2fc4fVert, v3ft2fc4fFrag, simple2dVert, simple2dFrag :: String
#include "baked.inc"
