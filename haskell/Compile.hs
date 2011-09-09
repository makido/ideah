module Compile (compile) where

import GHC
import HscTypes
import ErrUtils
import Bag
import Outputable
import MonadUtils
import SrcLoc
import Data.Maybe
import FastString
import Data.Char (isUpper)
import Data.List (isPrefixOf, partition)
import System.FilePath
import DriverPhases
import DriverPipeline hiding (compile)

compile outPath srcPath ghcPath compilerOptions exeFile files =
  let iOption                              = "-i" ++ outPath ++ ":" ++ srcPath
      (modules, nonModules)                = partition (isUpper . head . takeBaseName) files
      skipOut                              = null outPath
      runGhcPath additionalOpts outputOpts = runGhc (Just ghcPath)
        . doWalk (compilerOptions ++ additionalOpts ++
          (if skipOut then [] else outputOpts)) skipOut
  in do
      mapM_ (\mod -> runGhcPath ["--make", "-c", iOption] ["-outputdir " ++ outPath] [mod]) modules
      mapM_ (\file ->
          let ohiOption opt ext = opt ++ outPath ++ "/" ++ takeBaseName file ++ ext
          in runGhcPath [ "-c", iOption]
                        [ohiOption "-o " ".o", ohiOption "-ohi " ".hi"] [file])
        nonModules
      case exeFile of
        Just toExe -> runGhcPath []
            (("-o " ++ outPath ++ "/" ++ takeBaseName toExe ++ ".exe")
            : map ((++ ".o") . dropExtension) modules)
            [toExe]
        _          -> return ()

doWalk :: [String] -> Bool -> [String] -> Ghc ()
doWalk cmdFlags skipOut files = do
    flg <- getSessionDynFlags
    (flg, _, _) <- parseDynamicFlags flg (map noLoc cmdFlags)
    setSessionDynFlags $ if skipOut
       then flg { hscTarget = HscNothing, ghcLink = NoLink }
       else flg { ghcLink = NoLink }
--    hsc_env <- GHC.getSession
--    let srcs = zip files $ repeat Nothing
--    oneShot hsc_env StopLn srcs
    mapM_ (\file -> addTarget Target {
        targetId = TargetFile file Nothing
      , targetAllowObjCode = False
      , targetContents = Nothing })
      files
    loadWithLogger logger LoadAllTargets `gcatch` catcher
    warns <- getWarnings
    outputBag warns
    return ()

locStr :: SrcLoc -> String
locStr loc = if isGoodSrcLoc loc then show (srcLocLine loc) ++ ":"
  ++ show (srcLocCol loc) else "?"

spanStr :: SrcSpan -> String
spanStr span = locStr (srcSpanStart span) ++ "-"
  ++ locStr (srcSpanEnd span)

msgStr :: Message -> PrintUnqualified -> String
msgStr msg unqual = show $ msg (mkErrStyle unqual)

newMsgIndicator = "\f"

output1 :: (MonadIO m) => ErrMsg -> m ()
output1 msg = do
    let span   = head $ errMsgSpans msg
        unqual = errMsgContext msg
        printy = liftIO . putStrLn
        errMsg = msgStr (errMsgShortDoc msg) unqual
        isWarn = "Warning:" `isPrefixOf` errMsg
    printy newMsgIndicator
    printy $ fromMaybe "?" (fmap unpackFS $ srcSpanFileName_maybe span)
    printy $ (if isWarn then "W" else "E") ++ spanStr span
    printy $ msgStr (errMsgShortDoc msg) unqual
    printy $ msgStr (errMsgExtraInfo msg) unqual

outputBag :: (MonadIO m) => Bag ErrMsg -> m ()
outputBag msgs = do
    mapBagM output1 msgs
    return ()

output :: (MonadIO m) => SourceError -> m ()
output err = outputBag $ srcErrorMessages err

logger :: WarnErrLogger
logger Nothing = return ()
logger (Just err) = output err

catcher :: SourceError -> Ghc SuccessFlag
catcher err = do
    output err
    return Failed
