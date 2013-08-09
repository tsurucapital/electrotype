import Distribution.Simple
import System.Directory
import System.IO
import System.FilePath

internalPath, bakedPath, shadersDirPath :: FilePath
internalPath = "Graphics" </> "Rendering" </> "Electrotype" </> "Internal"
bakedPath = "Graphics" </> "Rendering" </> "Electrotype" </> "Baked"
shadersDirPath = "src" </> "shaders"

readVertAndFrag :: String -> IO (String, String)
readVertAndFrag shaderName = do
    vert <- readFile $ shadersDirPath </> shaderName ++ ".vert"
    frag <- readFile $ shadersDirPath </> shaderName ++ ".frag"
    return (vert, frag)

showVertAndFrag :: String -> (String, String) -> String
showVertAndFrag staticName (vert, frag) = "\n"
    ++ staticName ++ "Vert = " ++ show vert ++ "\n"
    ++ staticName ++ "Frag = " ++ show frag ++ "\n"

main = do
    defaultMainWithHooks $ simpleUserHooks
        { preBuild = \args buildflags -> do
            putStrLn "Baking shader strings..."
            template <- readFile $ bakedPath </> "Shaders.hs_template"
            shader1 <- readVertAndFrag "v3f-t2f-c4f"
            writeFile (bakedPath </> "Shaders.hs") $
                template ++ showVertAndFrag "v3ft2fc4f" shader1

            (preBuild simpleUserHooks) args buildflags
        }
