module Main where

import System.Environment
import Control.Monad
import System.Console.GetOpt
import Compile
import CheckMain
import Text.Parsec.Pos
import GHC

data Mode = Compile | CheckMain | GetIdType
    deriving Read

-- ./err_test
--    -m                  # err_test mode: establish presence of main function
--    -d                  # err_test mode: default (compile)
--    -g <path>           # ghc lib path
--    -o <path>           # output path
--    -s <path>           # source path
--    -c "<options>"      # compiler options
--    <files>             # files to be compiled

data Options = Options
    { mode            :: Mode
    , ghcPath         :: String
    , sourcePath      :: String
    , outputPath      :: String
    , compilerOptions :: [String]
    , position        :: SourcePos
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['m'] ["main-func-mode"] (ReqArg (\mod opt  -> opt {mode = read mod}) "Mode") "Mode"
    , Option ['g'] ["ghcpath"]        (ReqArg (\path opt -> opt {ghcPath = path}) "DIR") "GHC path"
    , Option ['o'] ["outpath"]        (ReqArg (\path opt -> opt {outputPath = path}) "DIR") "output path"
    , Option ['s'] ["sourcepath"]     (ReqArg (\path opt -> opt {sourcePath = path}) "DIR") "source path"
    , Option ['c'] ["ghcoptions"]     (ReqArg (\opts opt -> opt {compilerOptions = words opts}) "STRING") "GHC options"
    , Option "ln"  ["line-number"]    (ReqArg (\line opt -> opt {position = setSourceLine (position opt) (read line)}) "Num") "line number"
    , Option "col" ["column-number"]  (ReqArg (\col opt  -> opt {position = setSourceColumn (position opt) (read col)}) "Num") "column number"
    ]

defaultOpts :: Options
defaultOpts = Options
    { mode            = Compile
    , ghcPath         = ""
    , outputPath      = ""
    , sourcePath      = ""
    , compilerOptions = []
    , position        = newPos "" 0 0
    }

main = do
    args <- getArgs
    let (opts', files, errors)    = getOpt Permute options args
    unless (null $ errors) $ ioError $ userError $ concat errors
    let opts    = foldl (\opt f -> f opt) defaultOpts opts'
    let ghcpath = ghcPath opts
    let srcpath = sourcePath opts
    case mode opts of
        Compile  -> compile (outputPath opts) srcpath ghcpath (compilerOptions opts) files
        CheckMain -> runGhc (Just ghcpath) $ checkMain srcpath $ head files
        GetIdType -> return ()
