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
import Control.Monad (when)
import Data.Char (isUpper)
import System.Console.GetOpt
import System.FilePath.Posix (dropExtension)

locStr :: SrcLoc -> String
locStr loc = if isGoodSrcLoc loc then show (srcLocLine loc) ++ ":"
  ++ show (srcLocCol loc) else "?"

spanStr :: SrcSpan -> String
spanStr span = locStr (srcSpanStart span) ++ "-"
  ++ locStr (srcSpanEnd span)

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
    lifty $ show $ fromMaybe (mkFastString badSrcSpan)
          $ srcSpanFileName_maybe span
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
    setSessionDynFlags
      (flg { hscTarget = HscNothing, ghcLink = NoLink })
    mapM_ (\file -> addTarget Target {
        targetId = TargetFile file Nothing
      , targetAllowObjCode = False
      , targetContents = Nothing })
      files
    loadWithLogger logger LoadAllTargets `gcatch` catcher
    warns <- getWarnings
    outputBag warns
    return ()

-- ./err_test
--    -g <path>           # ghc path
--    -o <path>           # output path
--    -c "<options>"      # compiler options
--    -e <file>           # source file to be made executable
--    <files>             # files to be compiled

data Options = Options
  { ghcPath         :: String
  , outputPath      :: String
  , compilerOptions :: [String]
  , exeFile         :: Maybe String
  } deriving Show

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['g'] ["ghcpath"]    (ReqArg (\path opt -> opt {ghcPath = path}) "DIR")
    "GHC path"
  , Option ['o'] ["outpath"]    (ReqArg (\path opt ->
    opt {outputPath = path}) "DIR") "output path"
  , Option ['c'] ["ghcoptions"] (ReqArg (\opts opt ->
    opt {compilerOptions = words opts}) "STRING") "GHC options"
      -- need to delete quotes?
      -- STRING possible here?
  , Option ['e'] ["toexe"]      (ReqArg (\file opt -> opt {exeFile = Just file}) "FILE")
    "file to be make executable"
  ]

defaultOpts :: Options
defaultOpts = Options
  { ghcPath         = "C:/Program Files (x86)/Haskell Platform/2010.2.0.0/lib/"
  , outputPath      = "./"
  , compilerOptions = []
  , exeFile         = Nothing
  }

main = do
    args <- getArgs
    let (opts', files, errors)     = getOpt Permute options args
    when (not (null errors)) $ ioError $ userError $ concat errors
    let opts                      = foldl (\opt f -> f opt) defaultOpts opts'
        outPath                   = outputPath opts
        (modules, nonModules)     = foldl (\(ups, lows) file@(f:_)
          -> if isUpper f then (file:ups, lows) else (ups, file:lows))
          ([], []) files
        runGhcPath additionalOpts = runGhc (Just (ghcPath opts))
          . doWalk (compilerOptions opts ++ additionalOpts)
    runGhcPath ["--make", "-c", "-outputdir " ++ outPath] modules
    mapM (\file ->
      let ohiOption opt ext =
            opt ++ outPath ++ "/" ++ (dropExtension file) ++ ext
      in runGhcPath [ "-c", "-i" ++ outPath, ohiOption "-o " ".o"
                    , ohiOption "-ohi " ".hi"] [file]) nonModules
    case exeFile opts of
      Just toExe -> runGhcPath
        (("-o " ++ outPath ++ "/" ++ dropExtension toExe ++ ".exe")
        : map ((++ ".o") . dropExtension) modules)
        [toExe]
      _          -> return ()
    return ()
