module Graphics.Rendering.Electrotype.Internal.Shader
( getShaderPath
) where

import Data.Monoid
import Paths_electrotype (getDataFileName)

getShaderPath :: String -> IO (FilePath, FilePath)
getShaderPath name = do
    vertName <- getDataFileName $ "src/shaders/" <> name <> ".vert"
    fragName <- getDataFileName $ "src/shaders/" <> name <> ".frag"
    return (vertName, fragName)
