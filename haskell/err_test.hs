import GHC
import HscTypes
import ErrUtils
import Bag
import Outputable
import SrcLoc
import MonadUtils
import System.Environment

locStr :: SrcLoc -> String
locStr loc = if isGoodSrcLoc loc then show (srcLocLine loc) ++ ":" ++ show (srcLocCol loc) else "?"

spanStr :: SrcSpan -> String
spanStr span = locStr (srcSpanStart span) ++ "-" ++ locStr (srcSpanEnd span) ++ " " ++ show (srcSpanFileName_maybe span)

msgStr :: Message -> PrintUnqualified -> String
msgStr msg unqual = show $ msg (mkErrStyle unqual)

newMsgIndicator = "\f"

output1 :: (MonadIO m) => ErrMsg -> m ()
output1 msg = do
    liftIO $ putStrLn newMsgIndicator
    liftIO $ putStrLn $ spanStr $ head $ errMsgSpans msg
    let unqual = errMsgContext msg
    liftIO $ putStrLn $ msgStr (errMsgShortDoc msg) unqual
    liftIO $ putStrLn $ msgStr (errMsgExtraInfo msg) unqual

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
    --setSessionDynFlags (flg { hscTarget = HscNothing, ghcLink = NoLink })
    setSessionDynFlags flg
    mapM_ (\file -> addTarget Target { targetId = TargetFile file Nothing, targetAllowObjCode = False, targetContents = Nothing }) files
    loadWithLogger logger LoadAllTargets `gcatch` catcher
    warns <- getWarnings
    outputBag warns
    return ()

main = do
    args     <- getArgs
    runGhc (Just "C:/Haskell/lib") (doWalk ["-W"] args)
    return ()
