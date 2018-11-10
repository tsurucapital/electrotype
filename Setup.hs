import Distribution.Simple
import System.Directory
import System.IO
import System.FilePath

internalPath, bakedPath, shadersDirPath :: FilePath
internalPath = "Graphics" </> "Rendering" </> "Electrotype" </> "Internal"
bakedPath = "include"
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
            shader1 <- readVertAndFrag "v3f-t2f-c4f"
            shader2 <- readVertAndFrag "simple2d"
            writeFile (bakedPath </> "baked.inc") $
                showVertAndFrag "v3ft2fc4f" shader1 ++
                showVertAndFrag "simple2d" shader2

            (preBuild simpleUserHooks) args buildflags
        }
