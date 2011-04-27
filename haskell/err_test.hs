import GHC
import HscTypes
import ErrUtils
import Bag
import Outputable
import MonadUtils
import System.Environment
import SrcLoc
import Data.Maybe
import FastString

locStr :: SrcLoc -> String
locStr loc = if isGoodSrcLoc loc then show (srcLocLine loc) ++ ":" ++ show (srcLocCol loc) else "?"

spanStr :: SrcSpan -> String
spanStr span = locStr (srcSpanStart span) ++ "-" ++ locStr (srcSpanEnd span)

msgStr :: Message -> PrintUnqualified -> String
msgStr msg unqual = show $ msg (mkErrStyle unqual)

newMsgIndicator = "\f"
badSrcSpan      = "\b"

output1 :: (MonadIO m) => ErrMsg -> m ()
output1 msg = do
    let span   = head $ errMsgSpans msg
        unqual = errMsgContext msg
        lifty  = liftIO . putStrLn
    lifty newMsgIndicator
    lifty $ show $ fromMaybe (mkFastString badSrcSpan) $ srcSpanFileName_maybe span
    lifty $ spanStr span
    lifty $ msgStr (errMsgShortDoc msg) unqual
    lifty $ msgStr (errMsgExtraInfo msg) unqual

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

doWalk :: [String] -> [String] -> Ghc ()
doWalk cmdFlags files = do
    flg <- getSessionDynFlags
    (flg, _, _) <- parseDynamicFlags flg (map noLoc cmdFlags)
    setSessionDynFlags (flg { hscTarget = HscNothing, ghcLink = NoLink })
    mapM_ (\file -> addTarget Target { targetId = TargetFile file Nothing, targetAllowObjCode = False, targetContents = Nothing }) files
    loadWithLogger logger LoadAllTargets `gcatch` catcher
    warns <- getWarnings
    outputBag warns
    return ()

main = do
    args     <- getArgs
    runGhc (Just "C:/Program Files (x86)/Haskell Platform/2010.2.0.0/lib/") (doWalk ["-W"] args)
    return ()
