module Main where

import System.Environment
import Control.Monad (unless)
import System.Console.GetOpt
import Compile
import CheckMain
import GHC

data Mode = Compile | MainPres deriving Show

-- ./err_test
--    -m                  # err_test mode: establish presence of main function
--    -d                  # err_test mode: default (compile)
--    -g <path>           # ghc path
--    -o <path>           # output path
--    -s <path>           # source path
--    -c "<options>"      # compiler options
--    -e <file>           # source file to be made executable
--    <files>             # files to be compiled

data Options = Options
  { mode            :: Mode
  , ghcPath         :: String
  , sourcePath      :: String
  , outputPath      :: String
  , compilerOptions :: [String]
  , exeFile         :: Maybe String
  } deriving Show

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['m'] ["main-func-mode"] (NoArg (\opt -> opt {mode = MainPres})) "Mode"
  , Option ['d'] ["default-mode"]   (NoArg (\opt -> opt {mode = Compile})) "Mode"
  , Option ['g'] ["ghcpath"]        (ReqArg (\path opt -> opt {ghcPath = path}) "DIR")
    "GHC path"
  , Option ['o'] ["outpath"]        (ReqArg (\path opt ->
    opt {outputPath = path}) "DIR") "output path"
  , Option ['s'] ["sourcepath"]     (ReqArg (\path opt ->
    opt {sourcePath = path}) "DIR") "source path"
  , Option ['c'] ["ghcoptions"]     (ReqArg (\opts opt ->
    opt {compilerOptions = words opts}) "STRING") "GHC options"
  , Option ['e'] ["toexe"]          (ReqArg (\file opt -> opt {exeFile = Just file}) "FILE")
    "file to be make executable"
  ]

defaultOpts :: Options
defaultOpts = Options
  { mode            = Compile
  , ghcPath         = ""
  , outputPath      = ""
  , sourcePath      = ""
  , compilerOptions = []
  , exeFile         = Nothing
  }

main = do
  args <- getArgs
  let (opts', files, errors)    = getOpt Permute options args
  unless (null errors) $ ioError $ userError $ concat errors
  let opts    = foldl (\opt f -> f opt) defaultOpts opts'
  let ghcpath = ghcPath opts
  case mode opts of
    Compile  -> compile (outputPath opts) (sourcePath opts) ghcpath
                        (compilerOptions opts) (exeFile opts) files
    MainPres -> runGhc (Just ghcpath) $ checkMain $ head files
