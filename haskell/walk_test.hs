-- todo: type pretty-print (including typeclass context)
import Outputable
import GHC
import Var
import Name
import TypeRep
import HUtil
import Walker

walkVar :: Var -> String
walkVar var | isTyVar var = "Var.TyVar"
            | isTcTyVar var = "Var.TcTyVar"
--            | isId var = toString $ varType var
            | isId var = walkType (varType var)
            | otherwise = toString var

walkType :: Type -> String
--walkType (TyVarTy tyVar) = return ("TyVarTy " ++ toString (tyVarName tyVar) ++ ": " ++ show (tcTyVarDetails tyVar))
walkType (TyVarTy tyVar) = "TyVarTy " ++ toString tyVar
walkType (AppTy t1 t2) = let
    ts1 = walkType t1
    ts2 = walkType t2
  in "AppTy (" ++ ts1 ++ ", " ++ ts2 ++ ")"
walkType (TyConApp con types) = let
    ts = map walkType types
  in toString con ++ " " ++ show ts
walkType (FunTy t1 t2) = let
    ts1 = walkType t1
    ts2 = walkType t2
  in "(" ++ ts1 ++ " -> " ++ ts2 ++ ")"
walkType (ForAllTy var t) = "ForAllTy <" ++ toString var ++ "/" ++ walkType t ++ ">"
walkType (PredTy pt) = "PredTy {" ++ toString pt ++ "}"

test :: Id -> SrcSpan -> Where -> Ghc ()
--test var loc _ = trace ("----- " ++ varStr var) $ return ()
--    where varStr var = toString loc ++ " " ++ toString (Var.varName var) ++ ": " ++ toString (varType var) ++ " @ " ++ toString (varUnique var)
test var loc w = do
  let ts = walkType (varType var)
  trace (show w ++ ": " ++ (toString $ Var.varName var) ++ ": " ++ ts) $ return ()

test1 :: Name -> SrcSpan -> Where -> Ghc ()
test1 var loc w = trace ("----- " ++ varStr var) $ return ()
    where varStr var = toString loc ++ " " ++ toString (nameModule_maybe var) ++ " " ++ toString (nameOccName var) ++ ": " ++ toString (nameUnique var)

doWalk :: Ghc ()
doWalk = do
    flags <- getSessionDynFlags
    setSessionDynFlags flags
    addTarget Target { targetId = TargetFile "test.hs" Nothing, targetAllowObjCode = False, targetContents = Nothing }
    g <- depanal [] True
    parsed <- parseModule $ head g
    checked <- typecheckModule parsed
    --let (Just (grp, _, _, _)) = renamedSource checked
    --let vals = hs_valds grp
    --walkBinds test1 vals
    --trace (toString $ typecheckedSource checked) $ return ()
    walkLBinds CB {generic=test, name=test1} (typecheckedSource checked)

main = do
    runGhc (Just "C:\\Haskell\\lib") doWalk
    return ()
