module CheckMain where

import GHC
import Outputable
import MonadUtils
import DynFlags

checkMain srcpath file = do
  flags <- getSessionDynFlags
  (flags', _, _) <- parseDynamicFlags flags (map noLoc ["-i" ++ srcpath])
  setSessionDynFlags (flags' { hscTarget    = HscNothing
                             , ghcLink      = NoLink})
  addTarget Target
    { targetId = TargetFile file Nothing
    , targetAllowObjCode = False
    , targetContents = Nothing }
  (summary : _) <- depanal [] False
  parsedMod     <- parseModule summary
  let decls     = hsmodDecls $ unLoc $ parsedSource parsedMod
  let hasMain = any isMain $ map unLoc decls
  liftIO $ putStrLn $ if hasMain then "t" else "f"

isMain (ValD (FunBind funid _ _ _ _ _)) = showSDoc (ppr $ unLoc funid) == "main"
isMain _                                = False