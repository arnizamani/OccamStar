

module Instances where

import Language.Haskell.Syntax
import qualified Language.Haskell.Pretty as P
import Niz
import Data.List

data IP  = IP {getTag :: String, lhs :: Lhs, rhs :: Rhs, val :: Int}
type IP' = (String,String,String,Int)

type Language = String
type Width = Int
type Depth = Int
type Solution = Int
type Arity = Int
type Frequency = Int
type Comments = String
-- data Language = Boolean | List | Math | Logic | Stream | Analogy2 | Clause
--     deriving (Eq, Show)
type ConceptFile = FilePath
type IPFile = FilePath
type AgentMemory = ([Axiom],[Concept])
data Agent = Agent Comments (Width, Depth, Solution) AgentMemory

instance Size IP where
    size (IP _ x y _) = size x + size y

type Concept' = (Language,Arity,Frequency,String)
type Concept  = (Language,Arity,Frequency,HsExp)

type WM = HsExp
type Lhs = HsExp
type Rhs = HsExp
data Axiom = DArrow String Lhs Rhs  |  SArrow String Lhs Rhs
                deriving (Eq,Ord,Show)

instance Size Axiom where
    size (DArrow _ x y) = size x + size  y
    size (SArrow _ x y) = size x + size  y

axRhs :: Axiom -> Rhs
axLhs :: Axiom -> Lhs
axRhs (DArrow _ _ rhs) = rhs
axRhs (SArrow _ _ rhs) = rhs
axLhs (DArrow _ lhs _) = lhs
axLhs (SArrow _ lhs _) = lhs

-------------------------------------------------------------------------------
---------------- INSTANCE DECLARATIONS ----------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
----------------------  LENGTH OF HSEXP  --------------------------------------
-------------------------------------------------------------------------------


--class Size a where
--    size :: a -> Int
instance Size HsName where
    size _ = 1
instance Size HsQName where
    -- size (Special HsCons) = 0
    size _ = 1
instance Size HsLiteral where
    size (HsInt i) = length $ show i
    size _ = 1
instance Size HsQualType where
    size _ = 1
instance Size HsQOp where
    size (HsQVarOp x) = size x
    size (HsQConOp x) = size x
instance Size HsExp where
  size e = case e of
    HsVar n -> size n
    HsCon n -> size n
    HsLit x -> size x
    HsInfixApp e1 op e2 -> size e1 + size e2 + size op
    HsApp (HsVar _) e2 -> size e2 + 1
    HsApp (HsCon _) e2 -> size e2 + 1
    HsApp e1 e2 -> size e1 + size e2
    HsNegApp e -> 1 + size e
    HsLambda _ pats e -> size e + size pats
    HsLet decs e -> size e + size decs
    HsIf e1 e2 e3 -> 1 + size e1 + size e2 + size e3
    HsCase e alts -> size e + size alts
    HsDo stmts -> size stmts
    HsTuple es -> sum $ map size es
    HsList [] -> 1
    HsList es -> sum $ map size es -- size es
    HsParen e -> size e
    HsLeftSection e op -> size e + size op
    HsRightSection op e -> size e + size op
    HsRecConstr n fields -> size n + size fields
    HsRecUpdate e fields -> size e + size fields
    HsEnumFrom e -> 1 + size e
    HsEnumFromTo e1 e2 -> 1 + size e1 + size e2
    HsEnumFromThen e1 e2 -> 1 + size e1 + size e2
    HsEnumFromThenTo e1 e2 e3 -> 1 + size e1 + size e2 + size e3
    HsListComp e stmts -> size e + size stmts
    HsExpTypeSig _ e t -> size e + size t
    HsAsPat n e -> size n + size e
    HsWildCard -> 0
    HsIrrPat e -> size e
instance Size HsStmt where
    size s = case s of
        HsGenerator _ p e -> size p + size e
        HsQualifier e -> size e
        HsLetStmt decs -> size decs
instance Size HsPat where
  size p = case p of
    HsPVar n -> size n
    HsPLit l -> size l
    HsPNeg p -> 1 + size p
    HsPInfixApp p1 n p2 -> size p1 + size n + size p2
    HsPApp n pats -> 1 + (sum $ map size pats)
    HsPTuple pats -> sum $ map size pats
    HsPList [] -> 1
    HsPList pats -> sum $ map size pats
    HsPParen p -> size p
    HsPRec n fields -> size n + size fields
    HsPAsPat n p -> size n + size p
    HsPWildCard	-> 0
    HsPIrrPat p -> size p
instance Size HsPatField where
    size (HsPFieldPat n p) = size n + size p
instance Size HsDecl where
    size d = case d of
        HsTypeDecl _ name names htype -> size name + size names + size htype
        HsDataDecl _ ss name names cons qnames
                -> size name + size names + size cons + size qnames + size ss
        HsInfixDecl _ assoc _ ops -> 2 + size ops 
        HsNewTypeDecl _ c name names con qname -> size c + size name + size names + size con + size qname	 
        HsClassDecl _ c name names decs -> size c + size name + size names + size decs
        HsInstDecl _ c qname types decs -> size c + size qname + size types + size decs
        HsDefaultDecl _ types -> size types
        HsTypeSig _ names qtype -> size qtype + size names
        HsFunBind ms -> sum $ map size ms
        HsPatBind _ pat rhs decs -> size pat + size rhs + size decs
        HsForeignImport _ s1 safety s2 hname htype -> 3 + size hname + size htype
        HsForeignExport _ s1 s2 name htype -> 2 + size name + size htype
instance Size HsType where
    size t = case t of
        HsTyFun t1 t2 -> size t1 + size t2
        HsTyTuple types -> size types
        HsTyApp t1 t2 -> size t1 + size t2
        HsTyVar n -> size n
        HsTyCon n -> size n
instance (Size a, Size b) => Size (a,b) where
        size (a,b) = size a + size b
-- instance Size a => Size [a] where
--         size xs = sum (map size xs)
instance Size HsMatch where
    size (HsMatch _ name pats rhs decs) = 1 + sum (map size pats) + size rhs
instance Size HsRhs where
    size (HsUnGuardedRhs e) = size e
    size (HsGuardedRhss x) = size x
instance Size HsGuardedRhs where
    size (HsGuardedRhs _ e1 e2) = size e1 + size e2
instance Size HsConDecl where
    size (HsConDecl _ name list) = size name + size list
    size (HsRecDecl _ name list) = size name + size list
instance Size HsBangType where
    size (HsBangedTy   x) = size x
    size (HsUnBangedTy x) = size x
instance Size HsOp where
    size (HsVarOp  x) = size x
    size (HsConOp  x) = size x
instance Size HsAlt where
    size (HsAlt _ pat alts decs) = size pat + size alts + size decs
instance Size HsGuardedAlts where
    size (HsUnGuardedAlt exp) = size exp
    size (HsGuardedAlts xs) = size xs
instance Size HsGuardedAlt where
    size (HsGuardedAlt _ e1 e2) = size e1 + size e2
instance Size HsFieldUpdate where
    size (HsFieldUpdate qname exp) = size qname + size exp

instance Ord HsLiteral where
    compare (HsChar x) (HsChar y) = compare x y
    compare (HsChar       _) _ = LT
    compare _ (HsChar       _) = GT
    compare (HsString x) (HsString y) = compare x y
    compare (HsString     _) _ = LT
    compare _ (HsString     _) = GT
    compare (HsInt x) (HsInt y) = compare x y 
    compare (HsInt        _) _ = LT
    compare _ (HsInt        _) = GT
    compare (HsFrac x) (HsFrac y) = compare x y
    compare (HsFrac       _) _ = LT
    compare _ (HsFrac       _) = GT
    compare (HsCharPrim x) (HsCharPrim y) = compare x y
    compare (HsCharPrim   _) _ = LT
    compare _ (HsCharPrim   _) = GT
    compare (HsStringPrim x) (HsStringPrim y) = compare x y
    compare (HsStringPrim _) _ = LT
    compare _ (HsStringPrim _) = GT
    compare (HsIntPrim x) (HsIntPrim y) = compare x y
    compare (HsIntPrim    _) _ = LT
    compare _ (HsIntPrim    _) = GT
    compare (HsFloatPrim x) (HsFloatPrim y) = compare x y
    compare (HsFloatPrim  _) _ = LT
    compare _ (HsFloatPrim  _) = GT
    compare (HsDoublePrim x) (HsDoublePrim y) = compare x y
instance Ord HsPat where
    compare (HsPVar x) (HsPVar y) = compare x y
    compare (HsPVar _) _ = LT
    compare _ (HsPVar _) = GT
    compare (HsPLit x) (HsPLit y) = compare x y
    compare (HsPLit _) _ = LT
    compare _ (HsPLit _) = GT
    compare (HsPNeg x) (HsPNeg y) = compare x y
    compare (HsPNeg _) _ = LT
    compare _ (HsPNeg _) = GT
    compare (HsPInfixApp p1 n1 q1) (HsPInfixApp p2 n2 q2) | n1 == n2 = compare [p1,q1] [p2,q2]
    compare (HsPInfixApp _ n1 _) (HsPInfixApp _ n2 _) = compare n1 n2
    compare (HsPInfixApp _ _ _) _ = LT
    compare _ (HsPInfixApp _ _ _) = GT
    compare (HsPApp n1 p1) (HsPApp n2 p2) | n1 == n2 = compare p1 p2
    compare (HsPApp n1 _) (HsPApp n2 _) = compare n1 n2
    compare (HsPApp _ _) _ = LT
    compare _ (HsPApp _ _) = GT
    compare (HsPTuple p1) (HsPTuple p2) = compare p1 p2
    compare (HsPTuple _) _ = LT
    compare _ (HsPTuple _) = GT
    compare (HsPList p1) (HsPList p2) = compare p1 p2
    compare (HsPList _) _ = LT
    compare _ (HsPList _) = GT
    compare (HsPParen p1) (HsPParen p2) = compare p1 p2
    compare (HsPParen _) _ = LT
    compare _ (HsPParen _) = GT
    compare (HsPRec n1 p1) (HsPRec n2 p2) | n1 == n2 = compare p1 p2
    compare (HsPRec n1 _) (HsPRec n2 _) = compare n1 n2
    compare (HsPRec _ _) _ = LT
    compare _ (HsPRec _ _) = GT
    compare (HsPAsPat n1 p1) (HsPAsPat n2 p2) | n1 == n2 = compare p1 p2
    compare (HsPAsPat n1 _) (HsPAsPat n2 _) = compare n1 n2
    compare (HsPAsPat _ _) _ = LT
    compare _ (HsPAsPat _ _) = GT
    compare HsPWildCard _ = LT
    compare _ HsPWildCard = GT
    compare (HsPIrrPat x) (HsPIrrPat y) = compare x y

instance Ord HsPatField where
    compare (HsPFieldPat n1 p1) (HsPFieldPat n2 p2) | n1 == n2 = compare p1 p2
    compare (HsPFieldPat n1 _) (HsPFieldPat n2 _) = compare n1 n2
    
instance Ord HsExp where
    compare (HsLit x) (HsLit y) = compare x y
    compare (HsLit _) _ = LT
    compare _ (HsLit _) = GT
    compare (HsCon x) (HsCon y) = compare x y
    compare (HsCon _) _ = LT
    compare _ (HsCon _) = GT
    compare (HsVar x) (HsVar y) = compare x y
    compare (HsVar _) _ = LT
    compare _ (HsVar _) = GT
    compare (HsParen x) (HsParen y) = compare x y
    compare (HsParen _) _ = LT
    compare _ (HsParen _) = GT
    compare (HsNegApp x) (HsNegApp y) = compare x y
    compare (HsNegApp _) _ = LT
    compare _ (HsNegApp _) = GT
    compare (HsApp x1 x2) (HsApp y1 y2) = compare [x1,x2] [y1,y2]
    compare (HsApp _ _) _ = LT
    compare _ (HsApp _ _) = GT
    compare (HsTuple x) (HsTuple y) = compare x y
    compare (HsTuple _) _ = LT
    compare _ (HsTuple _) = GT
    compare (HsList x) (HsList y) = compare x y
    compare (HsList _) _ = LT
    compare _ (HsList _) = GT
    compare HsWildCard _ = LT
    compare _ HsWildCard = GT
    compare (HsAsPat n1 x) (HsAsPat n2 y) | n1 == n2 = compare x y
    compare (HsAsPat n1 _) (HsAsPat n2 _) = compare n1 n2
    compare (HsAsPat _ _) _ = LT
    compare _ (HsAsPat _ _) = GT
    compare (HsLambda a b c) (HsLambda x y z) | a == x && b == y = compare c z
    compare (HsLambda a b c) (HsLambda x y z) | a == x = compare b y
    compare (HsLambda a b c) (HsLambda x y z) = compare a x
    compare (HsLambda _ _ _) _ = LT
    compare _ (HsLambda _ _ _) = GT
    compare (HsInfixApp x y z) (HsInfixApp a b c) | y == b = compare [x,z] [a,c]
    compare (HsInfixApp x y z) (HsInfixApp a b c) = compare y b
    compare (HsInfixApp _ _ _) _ = LT
    compare _ (HsInfixApp _ _ _) = GT
    compare (HsLet a b) (HsLet x y) = compare (a,b) (x,y)
    compare (HsLet _ _) _ = LT
    compare _ (HsLet _ _) = GT
    compare (HsIf a b c) (HsIf x y z) = compare (a,b,c) (x,y,z)
    compare (HsIf _ _ _) _ = LT
    compare _ (HsIf _ _ _) = GT
    compare (HsCase a b) (HsCase x y) = compare (a,b) (x,y)
    compare (HsCase _ _) _ = LT
    compare _ (HsCase _ _) = GT
    compare (HsDo x) (HsDo y) = compare x y 
    compare (HsDo _) _ = LT
    compare _ (HsDo _) = GT
    compare (HsLeftSection a b) (HsLeftSection x y) = compare (a,b) (x,y)
    compare (HsLeftSection _ _) _ = LT
    compare _ (HsLeftSection _ _) = GT
    compare (HsRightSection a b) (HsRightSection x y) = compare (a,b) (x,y)
    compare (HsRightSection _ _) _ = LT
    compare _ (HsRightSection _ _) = GT
    compare (HsRecConstr a b) (HsRecConstr x y) = compare (a,b) (x,y)
    compare (HsRecConstr _ _) _ = LT
    compare _ (HsRecConstr _ _) = GT
    compare (HsRecUpdate a b) (HsRecUpdate x y) = compare (a,b) (x,y)
    compare (HsRecUpdate _ _) _ = LT
    compare _ (HsRecUpdate _ _) = GT
    compare (HsEnumFrom x) (HsEnumFrom y) = compare x y
    compare (HsEnumFrom _) _ = LT
    compare _ (HsEnumFrom _) = GT
    compare (HsEnumFromTo a b) (HsEnumFromTo x y) = compare (a,b) (x,y)
    compare (HsEnumFromTo _ _) _ = LT
    compare _ (HsEnumFromTo _ _) = GT
    compare (HsEnumFromThen a b) (HsEnumFromThen x y) = compare (a,b) (x,y)
    compare (HsEnumFromThen _ _) _ = LT
    compare _ (HsEnumFromThen _ _) = GT
    compare (HsEnumFromThenTo a b c) (HsEnumFromThenTo x y z) = compare (a,b,c) (x,y,z)
    compare (HsEnumFromThenTo _ _ _) _ = LT
    compare _ (HsEnumFromThenTo _ _ _) = GT
    compare (HsListComp a b) (HsListComp x y) = compare (a,b) (x,y)
    compare (HsListComp _ _) _ = LT
    compare _ (HsListComp _ _) = GT
    compare (HsExpTypeSig a b c) (HsExpTypeSig x y z) = compare (a,b,c) (x,y,z)
    compare (HsExpTypeSig _ _ _) _ = LT
    compare _ (HsExpTypeSig _ _ _) = GT
    compare (HsIrrPat x) (HsIrrPat y) = compare x y

instance Ord HsDecl where
    compare (HsTypeDecl a b c d) (HsTypeDecl w x y z) = compare (a,b,c,d) (w,x,y,z)
    compare (HsTypeDecl _ _ _ _) _ = LT
    compare _ (HsTypeDecl _ _ _ _) = GT
    compare (HsDataDecl a b c d e f) (HsDataDecl m n o p q r) = compare (a,b,c,d,e,f) (m,n,o,p,q,r)
    compare (HsDataDecl _ _ _ _ _ _) _ = LT
    compare _ (HsDataDecl _ _ _ _ _ _) = GT
    compare (HsInfixDecl a b c d) (HsInfixDecl w x y z) = compare (a,b,c,d) (w,x,y,z)
    compare (HsInfixDecl _ _ _ _) _ = LT
    compare _ (HsInfixDecl _ _ _ _) = GT
    compare (HsNewTypeDecl a b c d e f) (HsNewTypeDecl m n o p q r) = compare (a,b,c,d,e,f) (m,n,o,p,q,r)
    compare (HsNewTypeDecl _ _ _ _ _ _) _ = LT
    compare _ (HsNewTypeDecl _ _ _ _ _ _) = GT
    compare (HsClassDecl a b c d e) (HsClassDecl m n o p q) = compare (a,b,c,d,e) (m,n,o,p,q)
    compare (HsClassDecl _ _ _ _ _) _ = LT
    compare _ (HsClassDecl _ _ _ _ _) = GT
    compare (HsInstDecl a b c d e) (HsInstDecl m n o p q) = compare (a,b,c,d,e) (m,n,o,p,q)
    compare (HsInstDecl _ _ _ _ _) _ = LT
    compare _ (HsInstDecl _ _ _ _ _) = GT
    compare (HsDefaultDecl a b) (HsDefaultDecl x y) = compare (a,b) (x,y)
    compare (HsDefaultDecl _ _) _ = LT
    compare _ (HsDefaultDecl _ _) = GT
    compare (HsTypeSig a b c) (HsTypeSig x y z) = compare (a,b,c) (x,y,z)
    compare (HsTypeSig _ _ _) _ = LT
    compare _ (HsTypeSig _ _ _) = GT
    compare (HsFunBind a) (HsFunBind x) = compare a x
    compare (HsFunBind _) _ = LT
    compare _ (HsFunBind _) = GT
    compare (HsPatBind a b c d) (HsPatBind w x y z) = compare (a,b,c,d) (w,x,y,z)
    compare (HsPatBind _ _ _ _) _ = LT
    compare _ (HsPatBind _ _ _ _) = GT
    compare (HsForeignImport a b c d e f) (HsForeignImport m n o p q r) = compare (a,b,c,d,e,f) (m,n,o,p,q,r)
    compare (HsForeignImport _ _ _ _ _ _) _ = LT
    compare _ (HsForeignImport _ _ _ _ _ _) = GT
    compare (HsForeignExport a b c d e) (HsForeignExport m n o p q) = compare (a,b,c,d,e) (m,n,o,p,q)
    
instance Ord HsRhs where
    compare (HsUnGuardedRhs x) (HsUnGuardedRhs y) = compare x y
    compare (HsGuardedRhss x) (HsGuardedRhss y) = compare x y
instance Ord HsGuardedRhs where
    compare (HsGuardedRhs a b c) (HsGuardedRhs x y z) = compare (a,b,c) (x,y,z)
instance Ord HsMatch where
    compare (HsMatch a b c d e) (HsMatch m n o p q) = compare (a,b,c,d,e) (m,n,o,p,q)
instance Ord HsQualType where
    compare (HsQualType a b) (HsQualType x y) = compare (a,b) (x,y)
instance Ord HsType where
    compare (HsTyFun a b) (HsTyFun x y) = compare (a,b) (x,y)
    compare (HsTyFun _ _) _ = LT
    compare _ (HsTyFun _ _) = GT
    compare (HsTyTuple x) (HsTyTuple y) = compare x y
    compare (HsTyTuple _) _ = LT
    compare _ (HsTyTuple _) = GT
    compare (HsTyApp a b) (HsTyApp x y) = compare (a,b) (x,y)
    compare (HsTyApp _ _) _ = LT
    compare _ (HsTyApp _ _) = GT
    compare (HsTyVar x) (HsTyVar y) = compare x y
    compare (HsTyVar _) _ = LT
    compare _ (HsTyVar _) = GT
    compare (HsTyCon x) (HsTyCon y) = compare x y
instance Ord HsAssoc where
    compare HsAssocNone HsAssocNone = EQ
    compare HsAssocNone _ = LT
    compare _ HsAssocNone = GT
    compare HsAssocLeft HsAssocLeft = EQ
    compare HsAssocLeft _ = LT
    compare _ HsAssocLeft = GT
    compare HsAssocRight HsAssocRight = EQ

instance Ord HsConDecl where
    compare (HsConDecl a b c) (HsConDecl x y z) = compare (a,b,c) (x,y,z)
    compare (HsConDecl _ _ _) _ = LT
    compare _ (HsConDecl _ _ _) = GT
    compare (HsRecDecl a b c) (HsRecDecl x y z) = compare (a,b,c) (x,y,z)
instance Ord HsBangType where
    compare (HsBangedTy x) (HsBangedTy y) = compare x y
    compare (HsBangedTy _) _ = LT
    compare _ (HsBangedTy _) = GT
    compare (HsUnBangedTy x) (HsUnBangedTy y) = compare x y
instance Ord HsFieldUpdate where
    compare (HsFieldUpdate a b) (HsFieldUpdate x y) = compare (a,b) (x,y)
instance Ord HsStmt where
    compare (HsGenerator a b c) (HsGenerator x y z) = compare (a,b,c) (x,y,z)
    compare (HsGenerator _ _ _) _ = LT
    compare _ (HsGenerator _ _ _) = GT
    compare (HsQualifier x) (HsQualifier y) = compare x y
    compare (HsQualifier _) _ = LT
    compare _ (HsQualifier _) = GT
    compare (HsLetStmt x) (HsLetStmt y) = compare x y
instance Ord HsAlt where
    compare (HsAlt a b c d) (HsAlt w x y z) = compare (a,b,c,d) (w,x,y,z)
instance Ord HsGuardedAlts where
    compare (HsUnGuardedAlt x) (HsUnGuardedAlt y) = compare x y
    compare (HsUnGuardedAlt _) _ = LT
    compare _ (HsUnGuardedAlt _) = GT
    compare (HsGuardedAlts x) (HsGuardedAlts y) = compare x y
    
instance Ord HsGuardedAlt where
    compare (HsGuardedAlt a b c) (HsGuardedAlt x y z) = compare (a,b,c) (x,y,z)
    
class Pretty a where
    pretty :: a -> String

instance Pretty HsModule where
    pretty x = P.prettyPrint x
instance Pretty HsExportSpec where
    pretty x = P.prettyPrint x
instance Pretty HsImportDecl where
    pretty x = P.prettyPrint x
instance Pretty HsImportSpec where
    pretty x = P.prettyPrint x
instance Pretty HsAssoc where
    pretty x = P.prettyPrint x
instance Pretty HsDecl where
    pretty x = P.prettyPrint x
instance Pretty HsConDecl where
    pretty x = P.prettyPrint x
instance Pretty HsBangType where
    pretty x = P.prettyPrint x
instance Pretty HsMatch where
    --pretty (HsMatch pos f ps rhs whereDecls)
    -- =  pretty f ++ " " ++ concat (intersperse " " (map pretty ps)) ++ " = " ++ pretty rhs
    pretty x = P.prettyPrint x
instance Pretty HsRhs where
    pretty x = P.prettyPrint x
instance Pretty HsGuardedRhs where
    pretty x = P.prettyPrint x
instance Pretty HsSafety where
    pretty x = P.prettyPrint x
instance Pretty HsQualType where
    pretty x = P.prettyPrint x
instance Pretty HsType where
    pretty x = P.prettyPrint x
instance Pretty HsExp where
    pretty x = case x of
        HsVar name -> pretty name
        HsCon name -> pretty name
        HsList list -> (all isChar list)
                        ? (let s = [c | (HsLit (HsChar c)) <- list]
                           in P.prettyPrint (HsLit (HsString s)))
                        $ P.prettyPrint (HsList list)
        HsApp x arg -> pretty x ++ " (" ++ pretty arg ++ ")"
        HsNegApp e@(HsNegApp _) -> "-(" ++ pretty e ++ ")"
        HsNegApp e -> "-" ++ pretty e
        HsParen e -> "(" ++ pretty e ++ ")"
        HsInfixApp e1 (HsQConOp (Special HsCons)) (HsLit (HsInt i))
                  -> let p = pretty e1
                     in if take 1 p == "(" then p ++ ":" ++ show i else p ++ show i
        HsInfixApp e1 op e2 -> "(" ++ pretty e1 ++ P.prettyPrint op ++ pretty e2 ++ ")"
        
        e -> P.prettyPrint e
isChar (HsLit (HsChar _)) = True
isChar _ = False

instance Pretty HsStmt where
    pretty x = P.prettyPrint x
instance Pretty HsFieldUpdate where
    pretty x = P.prettyPrint x
instance Pretty HsAlt where
    pretty x = P.prettyPrint x
instance Pretty HsGuardedAlts where
    pretty x = P.prettyPrint x
instance Pretty HsGuardedAlt where
    pretty x = P.prettyPrint x
instance Pretty HsPat where
    pretty x = case x of
        (HsPInfixApp p1 op p2) -> "(" ++ pretty p1 ++ P.prettyPrint op ++ pretty p2 ++ ")"
        e -> P.prettyPrint e
instance Pretty HsPatField where
    pretty x = P.prettyPrint x
instance Pretty HsLiteral where
    pretty x = P.prettyPrint x
instance Pretty Module where
    pretty x = P.prettyPrint x
instance Pretty HsQName where
    pretty x = P.prettyPrint x
instance Pretty HsName where
    pretty x = P.prettyPrint x
instance Pretty HsQOp where
    pretty o = P.prettyPrint o
instance Pretty HsOp where
    pretty o = P.prettyPrint o
instance Pretty HsSpecialCon where
    pretty HsUnitCon = "()"
    pretty HsListCon = "[]"
    pretty HsFunCon = "->"
    pretty (HsTupleCon i) = "(" ++ take i (repeat ',') ++ ")"
    pretty HsCons = ":"
    
instance Pretty HsCName where
    pretty n = P.prettyPrint n

