module Graphics.Rendering.FreeTypeGL.Internal.Shader
( getShaderPath
) where

import Data.Monoid
import Paths_FreeTypeGL (getDataFileName)

getShaderPath :: String -> IO (FilePath, FilePath)
getShaderPath name = do
    vertName <- getDataFileName $ "src/shaders/" <> name <> ".vert"
    fragName <- getDataFileName $ "src/shaders/" <> name <> ".frag"
    return (vertName, fragName)
