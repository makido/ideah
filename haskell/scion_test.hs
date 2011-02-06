import Data.Maybe ( fromMaybe )
import Outputable
import GHC
import Debug.Trace
import Scion.Inspect.Find
import Scion.Inspect.TypeOf
import Scion.Inspect
import Scion.Utils
import PprTyThing
import FastString

toString :: (Outputable a) => a -> String
toString x = show $ ppr x defaultUserStyle

unqualifiedForModule' tcm = do
  fromMaybe alwaysQualify `fmap` mkPrintUnqualifiedForModule (moduleInfo tcm)

test1 checked p ty = do
  unqual <- unqualifiedForModule' checked
  let xxx = showSDocForUser unqual (prettyResult (fst p) Outputable.<+> dcolon Outputable.<+> pprTypeForUser True ty)
  trace (toString ty) $ return ()
  return ()

test :: Ghc SuccessFlag
test = do
  flags <- getSessionDynFlags
  setSessionDynFlags flags
  addTarget Target { targetId = TargetFile "test.hs" Nothing, targetAllowObjCode = False, targetContents = Nothing }
  g <- depanal [] True
  parsed <- parseModule $ head g
  let src = unLoc $ parsedSource parsed
  let decls = hsmodDecls src
  checked <- typecheckModule parsed
  let src = typecheckedSource checked
  let loc = srcLocSpan (mkSrcLoc (fsLit "test.hs") 4 1)
  let r = findHsThing (overlaps loc) src
  let (Just p) = pathToDeepest r
  let (Just ty) = typeOf p
  test1 checked p ty
  --ty <- exprType "(++ \"xyzzy\")"
  --trace (toStringT ty) $ return ()
  return Succeeded

main = do
  success <- runGhc (Just "C:\\Haskell\\lib") test
  print (case success of 
           Succeeded -> True
           _ -> False)
