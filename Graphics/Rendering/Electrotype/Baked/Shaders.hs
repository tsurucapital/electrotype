{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Electrotype.Baked.Shaders
( withTempShaderPaths
-- * Generated via Template Haskell.
, v3ft2fc4fVert, v3ft2fc4fFrag
, simple2dVert, simple2dFrag
) where

import System.FilePath ((</>))
import System.IO.Temp
import Graphics.Rendering.Electrotype.Baked.Shaders.TH

-- Inlines shaders into the code.
generateShaders
    [ ShaderInfo "v3ft2fc4f" "v3f-t2f-c4f"
    , ShaderInfo "simple2d" "simple2d"
    ]

withTempShaderPaths :: (FilePath -> (FilePath, FilePath) -> IO a) -> IO a
withTempShaderPaths f = withSystemTempDirectory "electrotype_shaders" $ \tmpPath -> do
    let vertPath = (tmpPath </> "v3f-2f-c4f.vert")
        fragPath = (tmpPath </> "v3f-2f-c4f.frag")
    writeFile vertPath v3ft2fc4fVert
    writeFile fragPath v3ft2fc4fFrag
    f tmpPath (vertPath, fragPath)
