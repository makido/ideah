module Compile (compile) where

import GHC
import HscTypes
import ErrUtils
import Bag
import Outputable
import MonadUtils
import SrcLoc
import Data.Char
import Data.Maybe
import FastString
import Data.List (isPrefixOf, partition)
import System.FilePath
import System.Directory
import System.Info (os)
import Control.Monad (when)

compile outPath srcPath ghcPath compilerOptions files =
    let 
        skipOut = null outPath

        options = compilerOptions ++ ["--make"] 
               ++ if skipOut then ["-i" ++ srcPath]
                             else ["-i" ++ outPath ++ ":" ++ srcPath, "-outputdir " ++ outPath]
        
        compileFile file = runGhc (Just ghcPath) (doWalk options skipOut [file])

        exeExtension = case os of
            "mingw32" -> "exe"
            _ -> ""

        outputExe srcFile = (exeFile, outPath </> relPath </> takeFileName exeFile)
            where exeFile = replaceExtension srcFile exeExtension
                  relPath = dropFileName $ makeRelative srcPath srcFile

        renameOutput srcFile = do
            let (exeFile, new) = outputExe srcFile
            exists <- doesFileExist exeFile
            when exists $ renameFile exeFile new

        clearOutput ext = do
            let file = outPath </> addExtension "Main" ext
            exists <- doesFileExist file
            when exists $ removeFile file

        needsRecompile srcFile = do
            let (_, exeFile) = outputExe srcFile
            exeExists <- doesFileExist exeFile
            if exeExists
                then do
                    srcModified <- getModificationTime srcFile
                    exeModified <- getModificationTime exeFile
                    return $ srcModified > exeModified
                else return True

        compileNonModule srcFile = 
            if skipOut 
                then compileFile srcFile
                else do
                    recompile <- needsRecompile srcFile
                    when recompile $ do
                        compileFile srcFile
                        clearOutput "o"
                        clearOutput "hi"
                        renameOutput srcFile

        (modules, nonModules) = partition (isUpper . head . takeBaseName) files
    in do
        mapM_ compileFile modules
        mapM_ compileNonModule nonModules

doWalk :: [String] -> Bool -> [String] -> Ghc ()
doWalk cmdFlags skipOut files = do
    flg <- getSessionDynFlags
    (flg, _, _) <- parseDynamicFlags flg (map noLoc cmdFlags)
    setSessionDynFlags $ if skipOut
       then flg { hscTarget = HscNothing, ghcLink = NoLink }
       else flg
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
locStr loc = if isGoodSrcLoc loc then 
                 show (srcLocLine loc) ++ ":" ++ show (srcLocCol loc) 
                 else "?"

spanStr :: SrcSpan -> String
spanStr span = locStr (srcSpanStart span) ++ "-" ++ locStr (srcSpanEnd span)

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
