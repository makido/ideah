module Walker (
    Where(..), Callback(..),
    walk, walkBinds, walkLBinds
    ) where

import Outputable
import Bag
import GHC
import BasicTypes
import DataCon

data Where = WTyDecl | WConDecl | WFunDecl | WFunDecl2 | WParam | WVal | WCon
    deriving Show

data Callback a m = CB {generic :: a -> SrcSpan -> Where -> m (),
                        name :: Name -> SrcSpan -> Where -> m ()}


walkId :: (Monad m, OutputableBndr a) => Callback a m -> Located a -> Where -> m ()
walkId f name definition = (generic f) (unLoc name) (getLoc name) definition


-- Type/class declarations
walkTyClD :: (Monad m, OutputableBndr a) => Callback a m -> TyClDecl a -> m ()
-- foreign type
walkTyClD f (ForeignType name _) = walkId f name WTyDecl
-- type family declaration
walkTyClD f (TyFamily _ name _ _) = walkId f name WTyDecl
-- data type declaration
walkTyClD f (TyData _ _ name _ _ _ cons _) = do
    walkId f name WTyDecl
    mapM_ walkCons cons
    where walkCons lcon = walkId f cname WConDecl
              where (ConDecl cname _ _ _ _ _ _ _) = unLoc lcon
-- type synonym declaration
walkTyClD f (TySynonym name _ _ _) = walkId f name WTyDecl
-- type class declaration
walkTyClD f (ClassDecl _ name _ _ _ _ _ _) = walkId f name WTyDecl


-- Instance declarations
walkInstD :: (Monad m, OutputableBndr a) => Callback a m -> InstDecl a -> m ()
walkInstD _f (InstDecl _ _ _ _) = return ()


-- Deriving declarations
walkDerivD :: (Monad m, OutputableBndr a) => Callback a m -> DerivDecl a -> m ()
walkDerivD _f (DerivDecl _) = return ()


walkBinds :: (Monad m, OutputableBndr a) => Callback a m -> HsValBindsLR a a -> m ()
walkBinds f (ValBindsIn binds _) = walkLBinds f binds
walkBinds f (ValBindsOut binds _) = mapM_ (walkLBinds f) bags
    where bags = map snd binds -- list of bags

walkLocals :: (Monad m, OutputableBndr a) => Callback a m -> HsLocalBinds a -> m ()
walkLocals f (HsValBinds binds) = walkBinds f binds
walkLocals f (HsIPBinds (IPBinds binds _)) = mapM_ walkIP binds
    where walkIP lip = do
              (generic f) id (getLoc lip) WParam
              walkLExpr f expr
              where (IPBind (IPName id) expr) = unLoc lip
walkLocals _f EmptyLocalBinds = return ()


walkLStmt :: (Monad m, OutputableBndr a) => Callback a m -> LStmt a -> m ()
walkLStmt f = walkStmt f . unLoc

walkStmt :: (Monad m, OutputableBndr a) => Callback a m -> Stmt a -> m ()
-- pat <- expr statement in do
walkStmt f (BindStmt pat expr _ _) = do
    walkLPattern f pat
    walkLExpr f expr
-- expr in do
walkStmt f (ExprStmt expr _ _) = walkLExpr f expr -- todo: has type
-- let in do
walkStmt f (LetStmt binds) = walkLocals f binds
-- ???
walkStmt f (ParStmt _) = return ()
walkStmt f (TransformStmt _ _ _) = return ()
walkStmt f (GroupStmt _ _) = return ()
walkStmt f (RecStmt _ _ _ _ _ _ _ _) = return ()


walkRHSs :: (Monad m, OutputableBndr a) => Callback a m -> GRHSs a -> m ()
walkRHSs f (GRHSs grhs locals) = do
        mapM_ walkRHS (map unLoc grhs)
        walkLocals f locals
    where walkRHS (GRHS stmts expr) = do
              mapM_ (walkLStmt f) stmts
              walkLExpr f expr


walkMatchGroup :: (Monad m, OutputableBndr a) => Callback a m -> MatchGroup a -> m ()
walkMatchGroup f (MatchGroup matches _) = mapM_ walkMatch (map unLoc matches)
    where walkMatch (Match pats _ rhss) = do
              mapM_ (walkLPattern f) pats
              walkRHSs f rhss


walkConPat :: (Monad m, OutputableBndr a) => Callback a m -> HsConPatDetails a -> m ()
walkConPat f details = mapM_ (walkLPattern f) (hsConPatArgs details)

walkLPattern :: (Monad m, OutputableBndr a) => Callback a m -> LPat a -> m ()
walkLPattern f lpat = walkPattern f (getLoc lpat) (unLoc lpat)

walkPattern :: (Monad m, OutputableBndr a) => Callback a m -> SrcSpan -> Pat a -> m ()
-- wildcard pattern (_)
walkPattern _f _loc (WildPat _) = return () -- todo: has type
-- variable pattern (matches any value)
walkPattern f loc (VarPat id) = (generic f) id loc WParam
-- ???
walkPattern f loc (VarPatOut id _) = (generic f) id loc WParam
-- lazy pattern
walkPattern f _loc (LazyPat pat) = walkLPattern f pat
-- as pattern (@)
walkPattern f _loc (AsPat id pat) = do
    walkId f id WParam
    walkLPattern f pat
-- parenthesis pattern (pat)
walkPattern f _loc (ParPat pat) = walkLPattern f pat
-- bang pattern
walkPattern f _loc (BangPat pat) = walkLPattern f pat
-- list pattern [a,b,c]
walkPattern f _loc (ListPat pats _) = mapM_ (walkLPattern f) pats -- todo: has type
-- tuple pattern (x, y, z)
walkPattern f _loc (TuplePat pats _ _) = mapM_ (walkLPattern f) pats -- todo: has type
-- parallel array pattern 
walkPattern f _loc (PArrPat pats _) = mapM_ (walkLPattern f) pats -- todo: has type
-- constructor pattern
walkPattern f _loc (ConPatIn id details) = do
    walkId f id WCon
    walkConPat f details
-- constructor pattern after typecheck
walkPattern f _loc (ConPatOut id _ _ _ details _) = do
    (name f) (dataConName $ unLoc id) (getLoc id) WCon
    walkConPat f details
-- view pattern
walkPattern f _loc (ViewPat expr pat _) = do
    walkLExpr f expr
    walkLPattern f pat
walkPattern _f _loc (QuasiQuotePat _) = return ()
-- literal pattern ("string")
walkPattern _f _loc (LitPat _) = return ()
-- numeric (or any overloaded literal) pattern
walkPattern _f _loc (NPat _ _ _) = return () -- todo: has type
-- n+k pattern
walkPattern _f _loc (NPlusKPat _ _ _ _) = return ()
walkPattern _f _loc (TypePat _) = return ()
walkPattern _f _loc (SigPatIn _ _) = return ()
walkPattern _f _loc (SigPatOut _ _) = return ()
walkPattern _f _loc (CoPat _ _ _) = return ()


walkLExpr :: (Monad m, OutputableBndr a) => Callback a m -> LHsExpr a -> m ()
walkLExpr f lexpr = walkExpr f (getLoc lexpr) (unLoc lexpr)

walkExpr :: (Monad m, OutputableBndr a) => Callback a m -> SrcSpan -> HsExpr a -> m ()
-- named reference
walkExpr f loc (HsVar var) = (generic f) var loc WVal
-- implicit parameter
walkExpr f loc (HsIPVar (IPName id)) = (generic f) id loc WVal
-- overloaded literal (number)
walkExpr _f _loc (HsOverLit _) = return () -- todo: has type
-- simple literal ("string", etc)
walkExpr _f _loc (HsLit _) = return ()
-- lambda expression (\pattern -> body)
walkExpr f _loc (HsLam mg) = walkMatchGroup f mg
-- function application (f x)
walkExpr f _loc (HsApp func param) = do
    walkLExpr f func
    walkLExpr f param
-- infix operation application (a + b)
walkExpr f _loc (OpApp left op _ right) = do
    walkLExpr f left
    walkLExpr f op
    walkLExpr f right
-- negation (-a)
walkExpr f _loc (NegApp expr _) = walkLExpr f expr
-- (expr)
walkExpr f _loc (HsPar expr) = walkLExpr f expr
-- (1+)
walkExpr f _loc (SectionL expr op) = do
    walkLExpr f expr
    walkLExpr f op
-- (+1)
walkExpr f _loc (SectionR op expr) = do
    walkLExpr f op
    walkLExpr f expr
-- case expression
walkExpr f _loc (HsCase expr mg) = do
    walkLExpr f expr
    walkMatchGroup f mg
-- if expression
walkExpr f _loc (HsIf expr ethen eelse) = do
    walkLExpr f expr
    walkLExpr f ethen
    walkLExpr f eelse
-- let expression
walkExpr f _loc (HsLet locals expr) = do
    walkLocals f locals
    walkLExpr f expr
-- do expression (incl. list comprehensions)
walkExpr f _loc (HsDo _ stmts expr _) = do -- todo: has type
    mapM_ (walkLStmt f) stmts
    walkLExpr f expr
-- list [a,b,c]
walkExpr f _loc (ExplicitList _ vals) = mapM_ (walkLExpr f) vals -- todo: has type
-- parallel array [:a,b,c:]
walkExpr f _loc (ExplicitPArr _ vals) = mapM_ (walkLExpr f) vals -- todo: has type
-- tuple (a, b, c)
walkExpr f _loc (ExplicitTuple args _) = mapM_ (walkLExpr f) (concatMap toExpr args)
    where toExpr (Present expr) = [expr]
          toExpr _ = []
-- record constructor { f1=e1, f2=e2 }
walkExpr f _loc (RecordCon con _ binds) = do
    walkId f con WCon
    walkRecord f binds
-- record update r { f1=e1, f2=e2 }
walkExpr f _loc (RecordUpd expr binds _ _ _) = do
    walkLExpr f expr
    walkRecord f binds
-- expr::Type
walkExpr f _loc (ExprWithTySig expr _) = walkLExpr f expr -- todo: has type
-- expr::Type after typecheck
walkExpr f _loc (ExprWithTySigOut expr _) = walkLExpr f expr -- todo: has type
-- list [a..b], [a..], [a,b..], [a,b..c]
walkExpr f _loc (ArithSeq _ si) = walkSeq f si
-- parallel array [:a..b:] or [:a,b..c:]
walkExpr f _loc (PArrSeq _ si) = walkSeq f si
-- SCC pragma
walkExpr f _loc (HsSCC _ expr) = walkLExpr f expr
-- core annotation
walkExpr f _loc (HsCoreAnn _ expr) = walkLExpr f expr
-- Template Haskell:
walkExpr _f _loc (HsBracket _) = return ()
walkExpr _f _loc (HsBracketOut _ _) = return ()
walkExpr _f _loc (HsSpliceE _) = return ()
walkExpr _f _loc (HsQuasiQuoteE _) = return ()
-- Arrows:
walkExpr _f _loc (HsProc _ _) = return ()
walkExpr _f _loc (HsArrApp _ _ _ _ _) = return ()
walkExpr _f _loc (HsArrForm _ _ _) = return ()
-- Hpc support:
walkExpr _f _loc (HsTick _ _ _) = return ()
walkExpr _f _loc (HsBinTick _ _ _) = return ()
walkExpr _f _loc (HsTickPragma _ _) = return ()
-- parser temporary:
walkExpr _f _loc EWildPat = return ()
walkExpr _f _loc (EAsPat _ _) = return ()
walkExpr _f _loc (EViewPat _ _) = return ()
walkExpr _f _loc (ELazyPat _) = return ()
walkExpr _f _loc (HsType _) = return ()
-- ???
walkExpr _f _loc (HsWrap wrapper expr) = return ()

walkSeq:: (Monad m, OutputableBndr a) => Callback a m -> ArithSeqInfo a -> m ()
walkSeq f (From from) = walkLExpr f from
walkSeq f (FromThen from thn) = do
    walkLExpr f from
    walkLExpr f thn
walkSeq f (FromTo from to) = do
    walkLExpr f from
    walkLExpr f to
walkSeq f (FromThenTo from thn to) = do
    walkLExpr f from
    walkLExpr f thn
    walkLExpr f to

walkRecord :: (Monad m, OutputableBndr a) => Callback a m -> HsRecordBinds a -> m ()
walkRecord f binds = mapM_ walkField (rec_flds binds)
    where walkField (HsRecField fld arg _) = do
              walkId f fld WVal
              walkLExpr f arg

walkLBinds :: (Monad m, OutputableBndr a) => Callback a m -> LHsBinds a -> m ()
walkLBinds f lbinds = do
    mapBagM (walkLBind f) lbinds
    return ()

walkLBind :: (Monad m, OutputableBndr a) => Callback a m -> LHsBindLR a a -> m ()
walkLBind f lbind = walkValD f (getLoc lbind) (unLoc lbind)

-- Normal declarations
walkValD :: (Monad m, OutputableBndr a) => Callback a m -> SrcSpan -> HsBind a -> m ()
-- function declaration (including value declaration)
walkValD f _loc (FunBind funId _ mg _ _ _) = do
    walkId f funId WFunDecl
    walkMatchGroup f mg
-- pattern declaration
walkValD f _loc (PatBind lhs rhss _ _) = do -- todo: has type
    walkLPattern f lhs
    walkRHSs f rhss
-- ???
walkValD f loc (VarBind var expr) = return ()
{-
  do
    (generic f) var loc WFunDecl
    walkLExpr f expr
-}
-- ???
walkValD f loc (AbsBinds _ _ exps binds) = do
    mapM (\id -> (generic f) id loc WFunDecl2) ids
    walkLBinds f binds
    where ids = [x | (_, x, _, _) <- exps]


-- Signature declarations
walkSigD :: (Monad m, OutputableBndr a) => Callback a m -> Sig a -> m ()
-- type signature
walkSigD _f (TypeSig _ _) = return ()
-- fixity
walkSigD _f (FixSig _) = return ()
-- ???
walkSigD _f (InlineSig _ _) = return ()
-- ???
walkSigD _f (SpecSig _ _ _) = return ()
-- ???
walkSigD _f (SpecInstSig _) = return ()
-- ???
walkSigD _f (IdSig _) = return ()


-- ???
walkDefD :: (Monad m, OutputableBndr a) => Callback a m -> DefaultDecl a -> m ()
walkDefD _f (DefaultDecl _) = return ()


-- Foreign declarations
walkForD :: (Monad m, OutputableBndr a) => Callback a m -> ForeignDecl a -> m ()
walkForD _f (ForeignImport _ _ _) = return ()
walkForD _f (ForeignExport _ _ _) = return ()


walk :: (Monad m, OutputableBndr a) => Callback a m -> SrcSpan -> HsDecl a -> m ()
walk f _loc (TyClD tyClD) = walkTyClD f tyClD
walk f _loc (InstD instD) = walkInstD f instD
walk f _loc (DerivD derivD) = walkDerivD f derivD
walk f loc (ValD valD) = walkValD f loc valD
walk f _loc (SigD sigD) = walkSigD f sigD
walk f _loc (DefD defD) = walkDefD f defD
walk f _loc (ForD forD) = walkForD f forD
walk f _loc (WarningD warnD) = return ()
walk f _loc (RuleD ruleD) = return ()
walk f _loc (SpliceD spliceD) = return ()
walk f _loc (DocD docD) = return ()
walk f _loc (AnnD annD) = return ()
