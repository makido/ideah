module GetIdType (getIdType) where

import Control.Monad (when)
import System.Exit

import GHC
import MonadUtils
import Outputable
import Var
import TypeRep

import HUtil
import Walker

unForall :: Type -> Type
unForall (ForAllTy _ t) = unForall t
unForall t = t

-- todo: find nearest id
extractTypes :: PprStyle -> Int -> Int -> Id -> SrcSpan -> Where -> Ghc ()
extractTypes style line col var loc WFunDecl2 = 
    when (isGoodSrcSpan loc && srcSpanStartLine loc == line && srcSpanStartCol loc == col) $ do
        let ts = show $ pprType (unForall $ varType var) style
        liftIO $ putStrLn ts
        liftIO $ exitSuccess
extractTypes _ _ _ _ _ _ = return ()

doExtractTypes line col checked = do
    let info = tm_checked_module_info checked
    (Just unqual) <- mkPrintUnqualifiedForModule info
    let style = mkUserStyle unqual AllTheWay
    walkLBinds CB { generic = extractTypes style line col, name = (\_ _ _ -> return ()) } (typecheckedSource checked)

doWalk srcPath srcFile line col = do
    setupFlags True ["-i" ++ srcPath]
    addTarget' srcFile
    load LoadAllTargets
    mods <- loadHsFile srcFile
    parsed <- parseModule $ head mods
    checked <- typecheckModule parsed
    doExtractTypes line col checked

getIdType srcPath ghcPath srcFile (line, col) = do
    runGhc (Just ghcPath) (doWalk srcPath srcFile line col)
    return ()
