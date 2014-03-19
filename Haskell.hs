{-
-- Module Haskell: Hasrkell code interpreting functions
-- Author: Abdul Rahim Nizamani, ITIT, Gothenburg University, Sweden
-- Started: 2013-08-23
-- Updated: 2013-09-28
-}

module Haskell where

import Instances
import Language.Haskell.Parser
import Language.Haskell.Syntax
import qualified Language.Haskell.Pretty as P
import Niz
import Data.List
import Data.Char
import Data.Either
import Data.Maybe (fromJust)
import Debug.Trace 

instance Show IP where
    show (IP tag lhs rhs val) = "(" ++ tag ++ showExp lhs ++ "," ++ showExp rhs ++ "," ++ show val ++ ")"

showAxiom :: Axiom -> String
showAxiom (DArrow s x y) = showExp x ++ " ->>_" ++ s ++ " " ++ showExp y
showAxiom (SArrow s x y) = showExp x ++ " ->_" ++ s ++ " " ++ showExp y

showExp :: HsExp -> String
showExp e = case e of
    (HsVar v) -> showQName v
    (HsCon c) -> showQName c
    (HsLit lit) -> showLit lit
    (HsInfixApp x o y) -> showExp' x ++ " " ++ showQOp o ++ " " ++ showExp' y
    -- (HsInfixApp x o y) -> show e
    (HsApp x y) -> showExp' x ++ " " ++ showExp' y
    (HsParen e) -> "(" ++ showExp e ++ ")"
    (HsTuple es) -> "(" ++ concat (intersperse "," (map showExp' es)) ++ ")"
    (HsList es) -> case (all isCharLit es && (not . null) es) of
        True  -> showExp $ HsLit (HsString [c | (HsLit (HsChar c)) <- es])
        False -> "[" ++ concat (intersperse "," (map showExp' es)) ++ "]"
    (HsNegApp e) -> "-(" ++ showExp e ++ ")"
    (HsWildCard) -> "_"
    (HsAsPat (HsIdent x) e) -> x ++ "@(" ++ showExp e ++ ")"
    (HsAsPat (HsSymbol x) e) -> x ++ "@(" ++ showExp e ++ ")"
    e            -> P.prettyPrint e
isCharLit (HsLit (HsChar _)) = True
isCharLit _ = False
-- print inner expressions, use brackets if necessary
showExp' :: HsExp -> String
showExp' e = case e of
    (HsVar v) -> showQName v
    (HsCon c) -> showQName c
    (HsLit lit) -> showLit lit
    (HsApp x y) -> "(" ++ showExp' x ++ " " ++ showExp' y ++ ")"
    (HsInfixApp x o y) -> "(" ++ showExp' x ++ " " ++ showQOp o ++ " " ++ showExp' y ++ ")"
    e -> showExp e
{-
    (HsLambda SrcLoc [HsPat] HsExp) -> 
    (HsLet [HsDecl] HsExp) -> 
    (HsIf HsExp HsExp HsExp) -> 
    (HsCase HsExp [HsAlt]) -> 
    (HsDo [HsStmt]) -> 
    (HsLeftSection HsExp HsQOp) -> 
    (HsRightSection HsQOp HsExp) -> 
    (HsRecConstr HsQName [HsFieldUpdate]) -> 
    (HsRecUpdate HsExp [HsFieldUpdate]) -> 
    (HsEnumFrom HsExp) -> 
    (HsEnumFromTo HsExp HsExp) -> 
    (HsEnumFromThen HsExp HsExp) -> 
    (HsEnumFromThenTo HsExp HsExp HsExp) -> 
    (HsListComp HsExp [HsStmt]) -> 
    (HsExpTypeSig SrcLoc HsExp HsQualType) -> 
    (HsIrrPat HsExp) -> 
-}
showQName (Qual (Module m) (HsIdent s))  = m ++ "." ++ s
showQName (Qual (Module m) (HsSymbol s)) = m ++ "." ++ s
showQName (UnQual (HsIdent s)) = s
showQName (UnQual (HsSymbol s)) = s
showQName (Special HsUnitCon) = "()"
showQName (Special HsListCon) = "[]"
showQName (Special HsFunCon) = "->"
showQName (Special (HsTupleCon i)) | i < 1 = "()"
showQName (Special (HsTupleCon i)) = "(" ++ replicate (i-1) ',' ++ ")"
showQName (Special HsCons) = ":"

showQOp (HsQVarOp (UnQual (HsIdent x))) = "`" ++ x ++ "`"
showQOp (HsQVarOp x) = showQName x
showQOp (HsQConOp x) = showQName x

showLit (HsChar c) = '\'' : c : '\'' : []
showLit (HsString s) = ('\"' : s) ++ "\""
showLit (HsInt i) = show i
showLit (HsFrac r) = show r
showLit (HsCharPrim c) = '\'' : c : '\'' : []
showLit (HsStringPrim s) = ('\"' : s) ++ "\""
showLit (HsIntPrim i) = show i
showLit (HsFloatPrim r) = show r
showLit (HsDoublePrim r) = show r

-- concatenate operator, also used as Juxtaposition in numbers
con = (HsQConOp (Special HsCons))   -- i.e.,     :


-------------------------------------------------------------------------------
-------- FUNCTIONS TO REPLACE A VARIABLE WITH AN EXP --------------------------
-------------------------------------------------------------------------------

-- | Replaces an HsPat with an HsExp (arg) in the main expression (exp)
replacePatExp :: Rhs -> HsExp -> HsExp -> HsExp
replacePatExp rhs (HsVar (UnQual (HsIdent (x:xs)))) (HsCon (UnQual c))
    | isUpper x && HsIdent (x:xs) == c
        = rhs
replacePatExp rhs (HsVar (UnQual (HsIdent (x:xs)))) (HsVar (UnQual c))
    | isUpper x && HsIdent (x:xs) == c
        = rhs
replacePatExp rhs (HsVar (Qual _ var)) arg = replaceAllSubExp rhs var arg
replacePatExp rhs (HsVar (UnQual var)) arg = replaceAllSubExp rhs var arg
replacePatExp rhs (HsNegApp p) (HsNegApp e) = replacePatExp rhs p e
replacePatExp rhs (HsParen p)  (HsParen e)  = replacePatExp rhs p e
replacePatExp rhs (HsParen p)  e           = replacePatExp rhs p e
replacePatExp rhs p            (HsParen e) = replacePatExp rhs p e
replacePatExp rhs (HsAsPat n2 p) (HsAsPat n1 e) | n1 == n2 = replacePatExp rhs p e
replacePatExp rhs (HsIrrPat p)   (HsIrrPat e)              = replacePatExp rhs p e
replacePatExp rhs (HsList p)  (HsList  e) | length p == length e
        = foldl (\rhs (p',e') -> replacePatExp rhs p' e') rhs $ zip p e
replacePatExp rhs (HsTuple p) (HsTuple e) | length p == length e
        = foldl (\rhs (p',e') -> replacePatExp rhs p' e') rhs $ zip p e

replacePatExp rhs (HsInfixApp p1 (HsQVarOp op1) p2) (HsInfixApp e1 (HsQVarOp op2) e2) | op1 == op2
        = replacePatExp (replacePatExp rhs p1 e1) p2 e2
replacePatExp rhs (HsInfixApp p1 (HsQConOp op1) p2) (HsInfixApp e1 (HsQConOp op2) e2) | op1 == op2
        = replacePatExp (replacePatExp rhs p1 e1) p2 e2
replacePatExp rhs (HsInfixApp p1 (HsQConOp (Special HsCons)) p2) (HsList (x:xs))
        = replacePatExp (replacePatExp rhs p1 x) p2 (HsList xs)
replacePatExp rhs (HsInfixApp p1 (HsQConOp (Special HsCons)) p2) (HsLit (HsString (x:xs)))
        = replacePatExp (replacePatExp rhs p1 (HsLit (HsChar x))) p2 (HsLit (HsString xs))
replacePatExp rhs (HsCon p) (HsCon e) | p == e
        = rhs
replacePatExp rhs (HsApp (HsCon p) x) (HsApp (HsCon e) y) | p == e
        = replacePatExp rhs x y
replacePatExp rhs _  _   = rhs

replaceAllSubExp :: HsExp -> HsName -> HsExp -> HsExp
replaceAllSubExp exp v arg = case exp of
    HsVar (UnQual v')  ->  v == v' ? arg $ exp
    HsVar (Qual _ v')  ->  v == v' ? arg $ exp
    HsInfixApp e1 q e2 ->  HsInfixApp (replaceAllSubExp e1 v arg) q (replaceAllSubExp e2 v arg)
    HsApp e1 e2        ->  HsApp (replaceAllSubExp e1 v arg) (replaceAllSubExp e2 v arg)
    HsNegApp e         ->  HsNegApp (replaceAllSubExp e v arg)
    HsLambda s p e     ->  HsLambda s p (replaceAllSubExp e v arg)
    HsLet decs e       ->  HsLet (map (\d -> replaceHsDecl d v arg) decs) (replaceAllSubExp e v arg)
    HsIf e1 e2 e3      ->  HsIf (replaceAllSubExp e1 v arg) (replaceAllSubExp e2 v arg) (replaceAllSubExp e3 v arg)
    HsCase e alts      ->  HsCase (replaceAllSubExp e v arg) (map (\a -> replaceHsAlt a v arg) alts)
    HsDo s             ->  HsDo (map (\s -> replaceHsStmt s v arg) s)
    HsTuple elist      ->  HsTuple (map (\e -> (replaceAllSubExp e v arg)) elist)
    HsList elist       ->  HsList  (map (\e -> (replaceAllSubExp e v arg)) elist)
    HsParen e          ->  HsParen (replaceAllSubExp e v arg)
    HsLeftSection e q  ->  HsLeftSection (replaceAllSubExp e v arg) q
    HsRightSection q e ->  HsRightSection q (replaceAllSubExp e v arg)
    HsRecConstr n fu   ->  HsRecConstr n (map (\(HsFieldUpdate n e) -> HsFieldUpdate n (replaceAllSubExp e v arg)) fu)
    HsRecUpdate e fu   ->  HsRecUpdate (replaceAllSubExp e v arg)
                                       (map (\(HsFieldUpdate n e)
                                                -> HsFieldUpdate n (replaceAllSubExp e v arg)) fu)
    HsEnumFrom e       ->  HsEnumFrom (replaceAllSubExp e v arg)
    HsEnumFromTo e f   ->  HsEnumFromTo (replaceAllSubExp e v arg) (replaceAllSubExp f v arg)
    HsEnumFromThen e f ->  HsEnumFromThen (replaceAllSubExp e v arg) (replaceAllSubExp f v arg)
    HsEnumFromThenTo e f g
                       ->  HsEnumFromThenTo (replaceAllSubExp e v arg)
                                            (replaceAllSubExp f v arg)
                                            (replaceAllSubExp g v arg)
    HsListComp e s     ->  HsListComp (replaceAllSubExp e v arg) (map (\s -> replaceHsStmt s v arg) s)
    HsExpTypeSig s e q ->  HsExpTypeSig s (replaceAllSubExp e v arg) q
    HsAsPat n e        ->  HsAsPat n (replaceAllSubExp e v arg)
    HsIrrPat e         ->  HsIrrPat (replaceAllSubExp e v arg)
    _                  ->  exp

replaceHsAlt :: HsAlt -> HsName -> HsExp -> HsAlt
replaceHsAlt (HsAlt sl p gs decs) v arg = HsAlt sl p gs (map (\d -> replaceHsDecl d v arg) decs)

replaceHsStmt ::  HsStmt -> HsName -> HsExp -> HsStmt
replaceHsStmt s v arg = case s of
    HsQualifier e -> HsQualifier (replaceAllSubExp e v arg)
    HsGenerator l p e -> HsGenerator l p (replaceAllSubExp e v arg)
    HsLetStmt decs -> HsLetStmt (map (\d -> replaceHsDecl d v arg) decs)

replaceHsDecl :: HsDecl -> HsName -> HsExp -> HsDecl
replaceHsDecl d v arg = case d of
    HsClassDecl l c n ns decs -> HsClassDecl l c n ns (map (\d -> replaceHsDecl d v arg) decs)
    HsInstDecl l c n ts decs -> HsInstDecl l c n ts (map (\d -> replaceHsDecl d v arg) decs)
    HsFunBind ms -> HsFunBind (map (\m -> replaceHsMatch m v arg) ms)
    HsPatBind l p rhs decs -> HsPatBind l p (replaceHsRhs rhs v arg)
                                            (map (\d -> replaceHsDecl d v arg) decs)
    _ -> d

replaceHsMatch :: HsMatch -> HsName -> HsExp -> HsMatch
replaceHsMatch (HsMatch sl n p rhs decs) v arg =
    HsMatch sl n p (replaceHsRhs rhs v arg) (map (\d -> replaceHsDecl d v arg) decs)

replaceHsRhs :: HsRhs  -> HsName -> HsExp -> HsRhs
replaceHsRhs (HsUnGuardedRhs e) v arg = HsUnGuardedRhs (replaceAllSubExp e v arg)
replaceHsRhs (HsGuardedRhss gs) v arg =
    HsGuardedRhss (map (\(HsGuardedRhs sl e f)
                            -> HsGuardedRhs sl (replaceAllSubExp e v arg)
                                               (replaceAllSubExp f v arg)) gs)
replaceHsGuardedAlts :: HsGuardedAlts -> HsName -> HsExp -> HsGuardedAlts
replaceHsGuardedAlts (HsUnGuardedAlt e) v arg = HsUnGuardedAlt (replaceAllSubExp e v arg)
replaceHsGuardedAlts (HsGuardedAlts gs) v arg =
    HsGuardedAlts (map (\(HsGuardedAlt sl e f)
                            -> HsGuardedAlt sl (replaceAllSubExp e v arg)
                                               (replaceAllSubExp f v arg)) gs)

convertInt :: Integer -> HsExp
convertInt i | i < 0 = HsNegApp (HsLit $ HsInt $ (i* (-1)))
convertInt i = HsLit (HsInt i)


-- | Normal form of expression
-- Removes all parentheses, converts strings to lists of chars, 
-- converts GHC literals to normal literals
-- first argument indicates if all variables should be transformed to string literals
normalExp :: Bool -> HsExp -> HsExp
normalExp b e = case e of
    HsVar v -> HsVar $ normalQName v
    HsCon c -> HsVar $ normalQName c
    HsLit (HsString s)      -> HsLit (HsString s)
    HsLit (HsStringPrim s)  -> HsLit (HsString s)
    HsLit (HsCharPrim c)    -> HsLit (HsChar c)
    HsLit (HsIntPrim i)     -> HsLit (HsInt i)
    HsLit (HsFloatPrim f)   -> HsLit (HsFrac f)
    HsLit (HsDoublePrim d)  -> HsLit (HsFrac d)
    HsInfixApp e1 op e2     -> HsInfixApp (normalExp b e1) (normalHsQOp op) (normalExp b e2)
    HsApp (HsVar x) e2      -> HsApp (HsVar x) (normalExp b e2)
    HsApp e1 e2             -> HsApp (normalExp b e1) (normalExp b e2)
    HsNegApp exp            -> HsNegApp (normalExp b exp)
    HsLambda loc pats exp   -> HsLambda loc (map (normalPat b) pats) (normalExp b exp)
    HsLet decs exp          -> HsLet (map (normalDec b) decs) (normalExp b exp)
    HsIf e1 e2 e3           -> HsIf (normalExp b e1) (normalExp b e2) (normalExp b e3)
    HsCase exp alts         -> HsCase (normalExp b exp) (map (normalAlt b) alts)
    HsDo stmts              -> HsDo (map (normalStmt b) stmts)
    HsTuple exps            -> HsTuple (map (normalExp b) exps)
    HsList  exps            -> HsList (map (normalExp b) exps)
    HsParen exp             -> normalExp b exp
    HsLeftSection exp op    -> HsLeftSection (normalExp b exp) op
    HsRightSection op exp   -> HsRightSection op (normalExp b exp)
    HsRecConstr n fs        -> HsRecConstr n (map (\(HsFieldUpdate n e) -> HsFieldUpdate n (normalExp b e)) fs)
    HsRecUpdate exp fs      -> HsRecUpdate (normalExp b exp) (map (\(HsFieldUpdate n e) -> HsFieldUpdate n (normalExp b e)) fs)
    HsEnumFrom exp          -> HsEnumFrom (normalExp b exp)
    HsEnumFromTo e1 e2      -> HsEnumFromTo (normalExp b e1) (normalExp b e2)
    HsEnumFromThen e1 e2    -> HsEnumFromThen (normalExp b e1) (normalExp b e2)
    HsEnumFromThenTo x y z  -> HsEnumFromThenTo (normalExp b x) (normalExp b y) (normalExp b z)
    HsListComp exp stmts    -> HsListComp (normalExp b exp) (map (normalStmt b) stmts)
    HsExpTypeSig loc exp qt -> HsExpTypeSig loc (normalExp b exp) qt
    HsAsPat name exp        -> HsAsPat name $ normalExp b exp
    HsIrrPat exp            -> HsIrrPat $ normalExp b exp
    exp                     -> exp

-- | Normalizes HsPat, removes parentheses, etc.
normalPat :: Bool -> HsPat -> HsPat
normalPat b p = case p of
    HsPVar (HsIdent s)      -> if b then HsPLit (HsString s) else p
    --HsPVar (HsSymbol s)      -> if b then HsPLit (HsString s) else p
    HsPLit (HsString s)     -> HsPList (map (HsPLit . HsChar) s)
    HsPLit (HsStringPrim s) -> HsPList (map (HsPLit . HsChar) s)
    HsPLit (HsCharPrim c)   -> HsPLit (HsChar c)
    HsPLit (HsIntPrim i)    -> HsPLit (HsInt i)
    HsPLit (HsFloatPrim f)  -> HsPLit (HsFrac f)
    HsPLit (HsDoublePrim d) -> HsPLit (HsFrac d)
    HsPNeg pat -> HsPNeg (normalPat b pat)
    HsPInfixApp (HsPInfixApp p1 (Special HsCons) p2) (Special HsCons) p3 ->
        normalPat b (HsPInfixApp p1 (Special HsCons) (HsPInfixApp p2 (Special HsCons) p3))
    HsPInfixApp p1 name p2 -> HsPInfixApp (normalPat b p1) name (normalPat b p2)
    HsPApp name pats -> HsPApp name (map (normalPat b) pats)
    HsPTuple pats -> HsPTuple (map (normalPat b) pats)
    HsPList pats -> HsPList (map (normalPat b) pats)
    HsPParen pat -> normalPat b pat
    HsPRec name fields -> HsPRec name (map (\(HsPFieldPat n p) -> HsPFieldPat n (normalPat b p)) fields)
    HsPAsPat name pat -> HsPAsPat name (normalPat b pat)
    HsPIrrPat p -> HsPIrrPat (normalPat b p)
    pat         -> pat

normalHsQOp :: HsQOp -> HsQOp
normalHsQOp (HsQVarOp qn) = HsQVarOp $ normalQName qn
normalHsQOp (HsQConOp qn) = HsQVarOp $ normalQName qn

normalQName :: HsQName -> HsQName
normalQName (Special HsCons) = (UnQual (HsSymbol ":"))
normalQName x = x

-- | Normalize  HsDecl
normalDec :: Bool -> HsDecl ->  HsDecl
normalDec b d = case d of
    HsClassDecl l c n ns decs -> HsClassDecl l c n ns (map (normalDec b) decs)
    HsInstDecl l c qn ts decs -> HsInstDecl l c qn ts (map (normalDec b) decs)
    HsFunBind ms -> HsFunBind (map (normalMatch b) ms)
    HsPatBind l pat rhs decs -> HsPatBind l (normalPat b pat) (normalRhs b rhs) (map (normalDec b) decs)
    dec    -> dec

normalMatch :: Bool -> HsMatch -> HsMatch 
normalMatch b (HsMatch l n pats rhs decs)
    = HsMatch l n (map (normalPat b) pats) (normalRhs b rhs) (map (normalDec b) decs)

normalAlt :: Bool -> HsAlt -> HsAlt
normalAlt b (HsAlt l pat galts decs) = HsAlt l (normalPat b pat) (normalAlt' galts) (map (normalDec b) decs)
  where
    normalAlt' (HsUnGuardedAlt e) = HsUnGuardedAlt (normalExp b e)
    normalAlt' (HsGuardedAlts alts) = HsGuardedAlts (map normalAlt'' alts)
    normalAlt'' (HsGuardedAlt l e1 e2) = HsGuardedAlt l (normalExp b e1) (normalExp b e2)

normalRhs :: Bool -> HsRhs -> HsRhs
normalRhs b (HsUnGuardedRhs exp) = HsUnGuardedRhs (normalExp b exp)
normalRhs b (HsGuardedRhss xs) = HsGuardedRhss (map (normalGRhs b) xs)
normalGRhs b (HsGuardedRhs l e1 e2) =  HsGuardedRhs l (normalExp b e1) (normalExp b e2)

normalStmt ::  Bool -> HsStmt ->  HsStmt 
normalStmt b (HsGenerator l pat exp) = HsGenerator l (normalPat b pat) (normalExp b exp)
normalStmt b (HsQualifier exp) = HsQualifier (normalExp b exp)
normalStmt b (HsLetStmt decs) = HsLetStmt (map (normalDec b) decs)

-- Matches a Pattern (HsPat) with an Expression (HsExp)
matchPatExp :: HsPat -> HsExp -> Bool
matchPatExp (HsPVar (HsIdent (x:xs))) (HsCon (UnQual c)) | isUpper x
        = HsIdent (x:xs) == c
matchPatExp (HsPVar (HsIdent (x:xs))) (HsVar (UnQual c)) | isUpper x
        = HsIdent (x:xs) == c

matchPatExp (HsPVar (HsIdent (v:vs))) arg | not (isUpper v) = True
matchPatExp (HsPLit p) (HsLit e) = p == e
matchPatExp (HsPNeg p) (HsNegApp e) = matchPatExp p e
matchPatExp (HsPInfixApp p1 op1 p2) (HsInfixApp e1 (HsQVarOp op2) e2) | op1 == op2
        = matchPatExp p1 e1 && matchPatExp p2 e2
matchPatExp (HsPInfixApp p1 op1 p2) (HsInfixApp e1 (HsQConOp op2) e2) | op1 == op2
        = matchPatExp p1 e1 && matchPatExp p2 e2
matchPatExp (HsPInfixApp p1 (Special HsCons) p2) (HsList (x:xs))
        = matchPatExp p1 x && matchPatExp p2 (HsList xs)
matchPatExp (HsPInfixApp p1 (Special HsCons) p2) (HsLit (HsString (x:xs)))
        = matchPatExp p1 (HsLit (HsChar x)) && matchPatExp p2 (HsLit (HsString xs))
matchPatExp (HsPTuple p) (HsTuple e) | length p == length e
        = all (uncurry matchPatExp) $ zip p e
matchPatExp (HsPList p) (HsList  e) | length p == length e
        = all (uncurry matchPatExp) $ zip p e
matchPatExp (HsPParen p) (HsParen e) = matchPatExp p e
matchPatExp (HsPParen p) e           = matchPatExp p e
matchPatExp p            (HsParen e) = matchPatExp p e
matchPatExp (HsPAsPat n2 p) (HsAsPat n1 e) | n1 == n2 = matchPatExp p e
matchPatExp HsPWildCard _ = True
matchPatExp (HsPIrrPat p) (HsIrrPat e) = matchPatExp p e
matchPatExp (HsPApp p []) (HsCon e) = p == e
matchPatExp (HsPApp p [p']) (HsApp (HsCon e) e') | p == e = matchPatExp p' e'
matchPatExp (HsPApp p [p']) (HsApp (HsVar e) e') | p == e = matchPatExp p' e'

matchPatExp _ _ = False

-- Matches a Pattern (HsPat) with another Pattern
matchPatPat :: HsPat -> HsPat -> Bool
matchPatPat (HsPVar v) (HsPVar w) = v == w
matchPatPat (HsPVar v) arg = True
matchPatPat arg (HsPVar v)  = True
matchPatPat HsPWildCard _ = True
matchPatPat _ HsPWildCard = True
matchPatPat (HsPLit p) (HsPLit e) = p == e
matchPatPat (HsPNeg p) (HsPNeg e) = matchPatPat p e
matchPatPat (HsPInfixApp p1 (Special HsCons) p2) (HsPList (x:xs))
        = matchPatPat p1 x && matchPatPat p2 (HsPList xs)
matchPatPat (HsPInfixApp p1 (Special HsCons) p2) (HsPLit (HsString (x:xs)))
        = matchPatPat p1 (HsPLit (HsChar x)) && matchPatPat p2 (HsPLit (HsString xs))
matchPatPat (HsPInfixApp p1 op1 p2) (HsPInfixApp e1 op2 e2) | op1 == op2
        = matchPatPat p1 e1 && matchPatPat p2 e2
--matchPatPat (HsPInfixApp p1 op1 p2) (HsPInfixApp e1 op2 e2) | op1 == op2
--        = matchPatPat p1 e1 && matchPatPat p2 e2
matchPatPat (HsPTuple p) (HsPTuple e) | length p == length e
        = all (uncurry matchPatPat) $ zip p e
matchPatPat (HsPList p) (HsPList  e) | length p == length e
        = all (uncurry matchPatPat) $ zip p e
matchPatPat (HsPParen p) (HsPParen e) = matchPatPat p e
matchPatPat (HsPParen p) e           = matchPatPat p e
matchPatPat p            (HsPParen e) = matchPatPat p e
matchPatPat (HsPAsPat n2 p) (HsPAsPat n1 e) | n1 == n2 = matchPatPat p e
matchPatPat (HsPIrrPat p) (HsPIrrPat e) = matchPatPat p e
matchPatPat (HsPApp p []) (HsPApp e []) = p == e
matchPatPat (HsPApp p ps) (HsPApp e es) = p == e && matchPatPat (HsPList ps) (HsPList es)
matchPatPat _ _ = False

expVarBinding :: HsExp -> HsExp -> [(HsQName,HsExp)]
expVarBinding (HsVar (Qual m v)) arg = [(Qual m v,arg)]
expVarBinding (HsNegApp p) (HsNegApp e) = expVarBinding p e
expVarBinding (HsInfixApp p1 (HsQVarOp op1) p2) (HsInfixApp e1 (HsQVarOp op2) e2) | op1 == op2
        = expVarBinding p1 e1 ++ expVarBinding p2 e2
expVarBinding (HsInfixApp p1 (HsQConOp op1) p2) (HsInfixApp e1 (HsQConOp op2) e2) | op1 == op2
        = expVarBinding p1 e1 ++ expVarBinding p2 e2
expVarBinding (HsInfixApp p1 (HsQConOp (Special HsCons)) p2) (HsList (x:xs))
        = expVarBinding p1 x ++ expVarBinding p2 (HsList xs)
expVarBinding (HsInfixApp p1 (HsQConOp (Special HsCons)) p2) (HsLit (HsString (x:xs)))
        = expVarBinding p1 (HsLit (HsChar x)) ++ expVarBinding p2 (HsLit (HsString xs))
expVarBinding (HsTuple p) (HsTuple e) | length p == length e
        = concatMap (uncurry expVarBinding) $ zip p e
expVarBinding (HsList p) (HsList  e) | length p == length e
        = concatMap (uncurry expVarBinding) $ zip p e
expVarBinding (HsParen p) (HsParen e) = expVarBinding p e
expVarBinding (HsParen p) e           = expVarBinding p e
expVarBinding p            (HsParen e) = expVarBinding p e
expVarBinding (HsAsPat n2 p) (HsAsPat n1 e) | n1 == n2 = expVarBinding p e
expVarBinding (HsIrrPat p) (HsIrrPat e) = expVarBinding p e
expVarBinding (HsApp (HsCon p) p') (HsApp (HsCon e) e') | p == e = expVarBinding p' e'
expVarBinding (HsApp (HsVar p) p') (HsApp (HsVar e) e') | p == e = expVarBinding p' e'
expVarBinding _ _ = []

-- | Returns multiple bindings for the same variable, if any exist
sameVarBinding :: [(HsQName,HsExp)] -> [(HsQName,HsExp)]
sameVarBinding [] = []
sameVarBinding ((name,exp):xs) =
    let xs' = [(n,e) | (n,e) <- xs, n == name]
    in if null xs'
            then sameVarBinding xs
            else ((name,exp):xs') ++ sameVarBinding [(n,e) | (n,e) <- xs, n /= name]
arityE :: HsDecl -> Bool
arityE exp = arity' exp 0
arity' = undefined

isHsPVar (HsPVar _) = True
isHsPVar _ = False
getHsPVar (HsPVar v) = v

isHsPLit (HsPLit _) = True
isHsPLit _ = False
getHsPLit (HsPLit l) = l


getRight (Right r) = r

getRhsExp :: HsRhs -> HsExp
getRhsExp (HsUnGuardedRhs e) = e
getRhsExp (HsGuardedRhss _) = error "error in function getRhsExp."

arityF :: [HsMatch] -> Bool
arityF = undefined
--arityF fs = all (== (head a)) a
--    where a = map (arityF') fs
--          arityF' = undefined

isFunDec :: HsDecl -> Bool
isFunDec (HsFunBind fs) = True
isFunDec _              = False

getMatches :: HsDecl -> [HsMatch]
getMatches (HsFunBind m) = m

getFunName :: HsMatch -> String
getFunName (HsMatch _ (HsIdent  s) _ _ _) = s
getFunName (HsMatch _ (HsSymbol s) _ _ _) = s

getHsRhs :: HsMatch -> HsRhs
getHsRhs (HsMatch _ _ _ r _) = r

getFunNames :: HsDecl -> [String]
getFunNames (HsFunBind m) = nub (map getFunName m)
getFunNames _ = []

{-
getExps :: HsDecl -> [HsExp]
getExps (HsFunBind []) = []
getExps (HsFunBind ms) = map getExps' ms
getExps (HsPatBind _ p _ _) = [patToExp p]
getExps _ = []
getExps' (HsMatch _ fname ps _ _) = foldl (\exp p -> HsApp exp (patToExp p)) (HsVar (UnQual fname)) ps
-}
getPats :: HsDecl -> [HsPat]
getPats (HsFunBind []) = []
getPats (HsFunBind ms) = concatMap getPats' ms
getPats (HsPatBind _ p _ _) = [p]
getPats _ = []
getPats' (HsMatch _ fname p _ _) = [HsPApp (UnQual fname) p]

isHsLit :: HsExp -> Bool
isHsLit (HsLit _) = True
isHsLit _ = False


isSolved = isGoal'

isGoal' :: HsExp -> Bool
isGoal' (HsLit _) = True
isGoal' (HsList list) = all isGoal' list
isGoal' (HsTuple list) = all isGoal' list
isGoal' (HsParen exp) = isGoal' exp
isGoal' (HsNegApp exp) = isGoal' exp
isGoal' (HsCon _) = True
isGoal' (HsInfixApp e (HsQConOp (Special HsCons)) (HsLit _)) = isGoal' e
isGoal' _ = False

-- read the value of a defined variable
getVariable :: HsModule -> String -> Maybe HsExp
getVariable (HsModule _ _ _ _ decs) name = get' decs name

get' :: [HsDecl] -> String -> Maybe HsExp
get' [] _ = Nothing
get' ((HsPatBind _ (HsPVar (HsIdent  n)) (HsUnGuardedRhs exp) _):xs) name | n == name = Just exp
get' ((HsPatBind _ (HsPVar (HsSymbol n)) (HsUnGuardedRhs exp) _):xs) name | n == name = Just exp
get' (_:xs) name = get' xs name

{-
-- | Converts an expression to a pattern
expToPat :: HsExp -> HsPat
expToPat exp = case (exp) of
    (HsVar (Qual mod name)) -> HsPVar name
    -- (HsVar (UnQual (n:name))) -> if isUpper n then HsPCon (n:name) else HsPVar (n:name)
    (HsVar (UnQual name)) -> HsPVar name
    (HsVar (Special HsCons)) -> error $ "(HsVar (Special HsCons))"
    (HsCon (UnQual n)) -> HsPVar n
    (HsLit literal) -> HsPLit literal
    (HsInfixApp e1 (HsQVarOp qn) e2) -> HsPInfixApp (expToPat e1) qn (expToPat e2)
    (HsInfixApp e1 (HsQConOp qn) e2) -> HsPInfixApp (expToPat e1) qn (expToPat e2)
    
    (HsApp (HsCon qn) e2) -> HsPApp qn [expToPat e2]
    (HsApp (HsVar qn) e2) -> HsPApp qn [expToPat e2]
    (HsNegApp exp) -> HsPNeg $ expToPat exp
    (HsLambda _ _ _) -> undefined
    (HsLet _ _) -> undefined
    (HsIf _ _ _) -> undefined
    (HsCase _ _) -> undefined
    (HsDo _) -> undefined
    (HsTuple exps) -> HsPTuple $ map expToPat exps
    (HsList exps) -> HsPList $ map expToPat exps
    (HsParen exp) -> HsPParen $ expToPat exp
    (HsLeftSection _ _) -> undefined
    (HsRightSection _ _) -> undefined
    (HsRecConstr _ _) -> undefined
    (HsRecUpdate _ _) -> undefined
    (HsEnumFrom _) -> undefined
    (HsEnumFromTo _ _) -> undefined
    (HsEnumFromThen _ _) -> undefined
    (HsEnumFromThenTo _ _ _) -> undefined
    (HsListComp _ _) -> undefined
    (HsExpTypeSig _ _ _) -> undefined
    (HsAsPat n exp) -> HsPAsPat n $ expToPat exp
    (HsWildCard) -> HsPWildCard
    (HsIrrPat exp) -> HsPIrrPat (expToPat exp)

-- | Converts a pattern to an expression
patToExp :: HsPat -> HsExp
patToExp (HsPLit lit) = HsLit lit
patToExp (HsPVar (HsIdent [])) = HsVar (UnQual (HsIdent []))
patToExp (HsPVar (HsIdent (':':xs))) = HsCon (UnQual (HsSymbol (':':xs)))
patToExp (HsPVar (HsIdent (x:xs))) | isUpper x = HsCon (UnQual (HsIdent (x:xs)))
patToExp (HsPVar (HsIdent xs)) = HsVar (UnQual (HsIdent xs))
patToExp (HsPVar (HsSymbol (':':xs))) = HsCon (UnQual (HsSymbol (':':xs)))
patToExp (HsPVar (HsSymbol xs)) = HsVar (UnQual (HsSymbol xs))
patToExp (HsPNeg pat) = HsNegApp (patToExp pat)
patToExp (HsPInfixApp p1 qname p2) = HsInfixApp (patToExp p1) (HsQVarOp qname) (patToExp p2)
patToExp (HsPApp (UnQual name) [p]) = HsApp (patToExp (HsPVar name)) (patToExp p)
patToExp (HsPApp qname ps) = foldl (\exp p -> HsApp exp (patToExp p)) (HsCon qname) ps
patToExp (HsPTuple pats) = HsTuple (map patToExp pats)
patToExp (HsPList pats) = HsList (map patToExp pats)
patToExp (HsPParen pat) = HsParen (patToExp pat)
patToExp (HsPRec qname fields)
    = HsRecConstr qname [HsFieldUpdate qn (patToExp pat) | (HsPFieldPat qn pat) <- fields]
patToExp (HsPAsPat name pat) = HsAsPat name (patToExp pat)
patToExp HsPWildCard = HsWildCard
patToExp (HsPIrrPat pat) = HsIrrPat (patToExp pat)
-}

getDecs :: HsModule -> [HsDecl]
getDecs (HsModule _ _ _ _ decs) = decs

getSubExpAx :: Axiom -> [HsExp]
getSubExpAx (DArrow _ x y) = getSubExp x ++ getSubExp y
getSubExpAx (SArrow _ x y) = getSubExp x ++ getSubExp y

-- | returns a list of all subexpressions
getSubExp :: HsExp -> [HsExp]
getSubExp e = e : case e of
    HsInfixApp e1 _ e2 -> e : (getSubExp e1 ++ getSubExp e2)
    HsApp (HsVar _) e2 -> e : getSubExp e2
    HsApp e1 e2 -> e : (getSubExp e1 ++ getSubExp e2)
    HsNegApp e1 -> e : (getSubExp e1)
    HsLambda _ _ e1 -> e : (getSubExp e1)
    HsLet decs e1 -> [e] ++ concatMap getSubExpDecl decs ++ getSubExp e1
    HsIf e1 e2 e3 -> e : (getSubExp e1 ++ getSubExp e2 ++ getSubExp e3)
    HsCase e1 alts -> [e] ++ getSubExp e1 ++ concatMap getSubExpAlt alts
    HsDo ss -> e : concatMap getSubExpStmt ss
    HsTuple es -> e : (concatMap getSubExp es)
    HsList es -> e : (concatMap getSubExp es)
    HsParen e1 ->  e : (getSubExp e1)
    HsLeftSection e1 _ -> e : (getSubExp e1)
    HsRightSection _ e1 -> e : (getSubExp e1)
    HsRecConstr _ fields -> e : (concatMap (\(HsFieldUpdate _ e1) -> getSubExp e1)fields)
    HsRecUpdate e1 fields -> [e] ++ (getSubExp e1) ++ (concatMap (\(HsFieldUpdate _ e2) -> getSubExp e2)fields)
    HsEnumFrom e1 -> e : (getSubExp e1)
    HsEnumFromTo e1 e2 -> e : (getSubExp e1 ++ getSubExp e2)
    HsEnumFromThen e1 e2 -> e : (getSubExp e1 ++ getSubExp e2)
    HsEnumFromThenTo e1 e2 e3 -> e : (getSubExp e1 ++ getSubExp e2 ++ getSubExp e3)
    HsListComp e1 ss -> [e] ++ getSubExp e1 ++ concatMap getSubExpStmt ss
    HsExpTypeSig _ e1 _ -> e : (getSubExp e1)
    HsAsPat _ e1 -> e : (getSubExp e1)
    HsIrrPat  e1 -> e : (getSubExp e1)
    _ -> [e]

getSubExpStmt :: HsStmt -> [HsExp]
getSubExpStmt (HsGenerator _ _ e) = getSubExp e
getSubExpStmt (HsQualifier e) = getSubExp e
getSubExpStmt (HsLetStmt decs) = concatMap getSubExpDecl decs

getSubExpDecl :: HsDecl -> [HsExp]
getSubExpDecl (HsFunBind matches) = concatMap getSubExpMatch matches
getSubExpDecl (HsPatBind _ _ rhs decs) = getSubExpRhs rhs ++ concatMap getSubExpDecl decs
getSubExpDecl (HsClassDecl _ _ _ _ decs) = concatMap getSubExpDecl decs
getSubExpDecl (HsInstDecl _ _ _ _ decs) = concatMap getSubExpDecl decs
getSubExpDecl _ = []

getSubExpRhs :: HsRhs -> [HsExp]
getSubExpRhs (HsUnGuardedRhs e) = getSubExp e
getSubExpRhs (HsGuardedRhss es) = concatMap (\(HsGuardedRhs _ e1 e2) -> getSubExp e1 ++ getSubExp e2) es

getSubExpMatch :: HsMatch -> [HsExp]
getSubExpMatch (HsMatch _ _ _ rhs decs) = getSubExpRhs rhs ++ concatMap getSubExpDecl decs

getSubExpAlt :: HsAlt -> [HsExp]
getSubExpAlt (HsAlt _ _ (HsUnGuardedAlt e) decs) = getSubExp e ++ concatMap getSubExpDecl decs
getSubExpAlt (HsAlt _ _ (HsGuardedAlts gs) decs)
    = concatMap (\(HsGuardedAlt _ e1 e2) -> getSubExp e1 ++ getSubExp e2) gs ++ concatMap getSubExpDecl decs

-- | returns all sub patterns of the given pattern
getSubPat :: HsPat -> [HsPat]
getSubPat p = case p of
    HsPNeg p1 -> p : getSubPat p1
    HsPInfixApp p1 _ p2 -> p : (getSubPat p1 ++ getSubPat p2)
    HsPApp _ ps -> p : concatMap getSubPat ps
    HsPTuple ps -> p : concatMap getSubPat ps
    HsPList ps -> p : concatMap getSubPat ps
    HsPParen p1 -> p : getSubPat p1
    HsPRec _ fields -> p : concatMap (\(HsPFieldPat _ p1) -> getSubPat p1) fields
    HsPAsPat _ p1 -> p : getSubPat p1
    HsPIrrPat p1 -> p : getSubPat p1
    _ -> [p]

-- special equal instance, for ignoring HsQVarOp and HsQConOp difference
equalExp :: (HsExp, HsExp) -> Bool
equalExp e = 
  case e of
    (HsVar n1,HsCon n2) -> n1 == n2
    (HsCon n1,HsVar n2) -> n1 == n2
    (HsInfixApp e11 o1 e12,HsInfixApp e21 o2 e22)
                        -> equalExp(e11,e21) && equalExp(e12,e22) && equalQOp o1 o2
    (HsApp e1 e2,HsApp f1 f2) -> equalExp (e1,f1) && equalExp (e2,f2)
    (HsNegApp x,HsNegApp y) -> equalExp (x,y)
    (HsLambda _ p1 e1,HsLambda _ p2 e2) -> p1 == p2 && equalExp (e1,e2)
    (HsLet d1 e1,HsLet d2 e2) -> d1 == d2 && equalExp(e1,e2)
    (HsIf x1 x2 x3,HsIf y1 y2 y3) -> equalExp(x1,y1) && equalExp(x2,y2) && equalExp(x3,y3)
    (HsCase e1 a1,HsCase e2 a2) -> equalExp(e1,e2) && a1 == a2
    (HsTuple e1,HsTuple e2) -> if length e1 /= length e2
                                    then False
                                    else all equalExp $ zip e1 e2
    (HsList e1,HsList e2) -> if length e1 /= length e2
                                    then False
                                    else all equalExp $ zip e1 e2
    (HsParen e1,HsParen e2) -> equalExp (e1,e2)
    (HsParen e1,e2) -> equalExp (e1,e2)
    (e1,HsParen e2) -> equalExp (e1,e2)
    (HsLeftSection e1 q1,HsLeftSection e2 q2) -> equalExp(e1,e2) && equalQOp q1 q2
    (HsRightSection q1 e1,HsRightSection q2 e2) -> equalExp(e1,e2) && equalQOp q1 q2
    (HsRecUpdate e1 f1,HsRecUpdate e2 f2) -> equalExp(e1,e2) && f1 == f2
    (HsEnumFrom e1,HsEnumFrom e2) -> equalExp(e1,e2)
    (HsEnumFromTo e1 e2,HsEnumFromTo f1 f2) -> equalExp(e1,f1) && equalExp(e2,f2)
    (HsEnumFromThen e1 e2,HsEnumFromThen f1 f2) -> equalExp(e1,f1) && equalExp(e2,f2)
    (HsEnumFromThenTo e1 e2 e3,HsEnumFromThenTo f1 f2 f3)
                -> equalExp(e1,f1) && equalExp(e2,f2) && equalExp(e3,f3)
    (HsListComp e1 s1,HsListComp e2 s2) -> s1 == s2 && equalExp(e1,e2)
    (HsExpTypeSig _ e1 q1,HsExpTypeSig _ e2 q2) -> q1 == q2 && equalExp(e1,e2)
    (HsAsPat n1 e1,HsAsPat n2 e2) -> n1 == n2 && equalExp(e1,e2)
    (HsIrrPat e1,HsIrrPat e2) -> equalExp(e1,e2)
    (x,y) -> x == y

equalQOp :: HsQOp -> HsQOp -> Bool
equalQOp (HsQVarOp x) (HsQConOp y) = x == y
equalQOp (HsQConOp x) (HsQVarOp y) = x == y
equalQOp x y = x == y

