module CheckMain (checkMain) where

import GHC
import MonadUtils

import HUtil

checkMain srcpath file = do
    setupFlags True ["-i" ++ srcpath]
    addTarget' file
    summy <- loadHsFile file
    parsedMod      <- parseModule $ head summy
    let decls      = hsmodDecls $ unLoc $ parsedSource parsedMod
        hasMain    = any isMain $ map unLoc decls
    liftIO $ putStrLn $ if hasMain then "t" else "f"

isMain (ValD (FunBind funid _ _ _ _ _)) = toString (unLoc funid) == "main"
isMain _                                = False
