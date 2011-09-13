module HUtil where

import System.FilePath (equalFilePath)
import System.Directory (canonicalizePath)
import Control.Monad (filterM)

import GHC
import Outputable
import MonadUtils
import PprTyThing

toString :: (Outputable a) => a -> String
toString x = show $ ppr x defaultUserStyle

toStringT :: Type -> String
toStringT t = show $ pprTypeForUser True t defaultUserStyle

unsupported :: String -> a -> a
unsupported str x = error str `seq` x

setupFlags skipOut cmdFlags = do
    flg <- getSessionDynFlags
    (flg, _, _) <- parseDynamicFlags flg (map noLoc cmdFlags)
    setSessionDynFlags $ if skipOut
       then flg { hscTarget = HscNothing, ghcLink = NoLink }
       else flg


addTarget' file = 
    addTarget Target { targetId           = TargetFile file Nothing
                     , targetAllowObjCode = False
                     , targetContents     = Nothing }

loadHsFile file = do
    summaries <- depanal [] False
    filterM (\sum -> do
          absoluteSummary <- liftIO $ canonicalizePath $ ms_hspp_file sum
          absoluteFile    <- liftIO $ canonicalizePath file
          return $ equalFilePath absoluteFile absoluteSummary)
        summaries
