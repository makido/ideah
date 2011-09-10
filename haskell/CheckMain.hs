module CheckMain where

import GHC
import Outputable
import MonadUtils
import DynFlags
import System.FilePath (equalFilePath)
import System.Directory (canonicalizePath)
import Control.Monad (filterM)

checkMain srcpath file = do
    flags          <- getSessionDynFlags
    (flags', _, _) <- parseDynamicFlags flags (map noLoc ["-i" ++ srcpath])
    setSessionDynFlags (flags'
        { hscTarget          = HscNothing
        , ghcLink            = NoLink})
    addTarget Target
        { targetId           = TargetFile file Nothing
        , targetAllowObjCode = False
        , targetContents     = Nothing }
    summaries <- depanal [] False
    summy <- filterM (\sum -> do
          absoluteSummary <- liftIO $ canonicalizePath $ ms_hspp_file sum
          absoluteFile    <- liftIO $ canonicalizePath file
          return $ equalFilePath absoluteFile absoluteSummary)
        summaries
    parsedMod      <- parseModule $ head summy
    let decls      = hsmodDecls $ unLoc $ parsedSource parsedMod
    let hasMain    = any isMain $ map unLoc decls
    liftIO $ putStrLn $ if hasMain then "t" else "f"

isMain (ValD (FunBind funid _ _ _ _ _)) = showSDoc (ppr $ unLoc funid) == "main"
isMain _                                = False
