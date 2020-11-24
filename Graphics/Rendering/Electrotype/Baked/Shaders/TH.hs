{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Electrotype.Baked.Shaders.TH
    ( generateShaders
    , ShaderInfo(..)
    ) where


import System.FilePath
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

shadersDirPath :: FilePath
shadersDirPath = "src" </> "shaders"

readVertAndFrag :: String -> Q (String, String)
readVertAndFrag shaderName = do
    let vertFile = shadersDirPath </> shaderName <.> "vert"
        fragFile = shadersDirPath </> shaderName <.> "frag"
    -- Have GHC recompile if underlying shader files change.
    addDependentFile vertFile
    addDependentFile fragFile
    runIO $ (,) <$> readFile vertFile <*> readFile fragFile

-- showVertAndFrag :: String -> (String, String) -> String
-- showVertAndFrag staticName (vert, frag) = "\n"
--     ++ staticName ++ "Vert = " ++ show vert ++ "\n"
--     ++ staticName ++ "Frag = " ++ show frag ++ "\n"

data ShaderInfo = ShaderInfo
    { siName :: String
    -- ^ Name of the shader. Used when generating the Haskell code.
    , siFile :: String
    -- ^ File name (without extension) of the shader.
    }

generateShaders :: [ShaderInfo] -> DecsQ
generateShaders = fmap concat . mapM generateShader

generateShader :: ShaderInfo -> DecsQ
generateShader shader = do
    (vert, frag) <- readVertAndFrag $ siFile shader
    strType <- [t| String |]
    let vertName = mkName $ siName shader <> "Vert"
        fragName = mkName $ siName shader <> "Frag"
        strFun n content =
            [ SigD n strType
            , FunD n [Clause mempty (NormalB (LitE (StringL content))) mempty]
            ]
    pure $ strFun vertName vert <> strFun fragName frag

    -- main = do
    -- defaultMainWithHooks $ simpleUserHooks
    --     { preBuild = \args buildflags -> do
    --         error $ show buildflags
    --         putStrLn "Baking shader strings..."
    --         shader1 <- readVertAndFrag "v3f-t2f-c4f"
    --         shader2 <- readVertAndFrag "simple2d"
    --         writeFile (bakedPath </> "baked.inc") $
    --             showVertAndFrag "v3ft2fc4f" shader1 ++
    --             showVertAndFrag "simple2d" shader2

    --         (preBuild simpleUserHooks) args buildflags
    --     }
