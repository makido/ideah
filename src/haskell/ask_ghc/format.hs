import System.Environment
import MonadUtils
import GHC

locStr :: SrcLoc -> String
locStr loc = if isGoodSrcLoc loc then show (srcLocLine loc) ++ ":" ++ show (srcLocCol loc) else "?"

spanStr :: SrcSpan -> String
spanStr span = locStr (srcSpanStart span) ++ "-" ++ locStr (srcSpanEnd span)

prLoc :: (MonadIO m) => String -> Located a -> m ()
prLoc what loc = liftIO $ putStrLn $ what ++ ": " ++ (spanStr $ getLoc loc)

doWalk :: [String] -> Ghc ()
doWalk files = do
    flg <- getSessionDynFlags
    --(flg, _, _) <- parseDynamicFlags flg (map noLoc ["--make"])
    setSessionDynFlags (flg { hscTarget = HscNothing, ghcLink = NoLink })
    mapM_ (\file -> addTarget Target { targetId = TargetFile file Nothing, targetAllowObjCode = False, targetContents = Nothing }) files
    g <- depanal [] True
    parsed <- parseModule $ head g
    let md = unLoc $ pm_parsed_source parsed
{-
    case hsmodName md of
        Just mn -> liftIO $ putStrLn $ spanStr $ getLoc mn
        _ -> return ()
-}
    mapM_ (prLoc "import") (hsmodImports md)
    mapM_ (prLoc "decl") (hsmodDecls md)
    return ()

main = do
    args <- getArgs
    runGhc (Just "C:\\Haskell\\lib") (doWalk args)
    return ()
