module CheckMain where

import GHC
import Outputable
import MonadUtils

newMsgIndicator = "\f"

checkMain file = do
  flags <- getSessionDynFlags
  setSessionDynFlags (flags { hscTarget = HscNothing, ghcLink = NoLink })
  addTarget Target
    { targetId = TargetFile file Nothing
    , targetAllowObjCode = False
    , targetContents = Nothing }
  (summary : _) <- depanal [mkModuleName file] True
  parsedMod     <- parseModule summary
  let decls     = hsmodDecls $ unLoc $ parsedSource parsedMod
  let hasMain = any isMain $ map unLoc decls
  liftIO $ putStrLn $ newMsgIndicator ++ (if hasMain then "t" else "f")

isMain (ValD (FunBind funid _ _ _ _ _)) = showSDoc (ppr $ unLoc funid) == "main"
isMain _               = False
