{-
-- Module Interpreter:  functions to interpret haskell code
-- Author: Abdul Rahim Nizamani, ITIT, Gothenburg University, Sweden
-- Started: 2013-08-23
-- Updated: 2014-03-12

Most general type:
  Dig ->> Num
  Num ->> Aterm
  
  So suggest the most general axiom:
  Aterm.x + 0 ->> Aterm.x

Abstraction (not done)
At d > 6, only consider axioms that have same variables on both sides.

Free variables allowed: 1

-}

module Interpreter where

import Instances
import Data.Graph.AStar
import Haskell
import Language.Haskell.Parser
import Language.Haskell.Syntax
import qualified Language.Haskell.Pretty as P
import Niz
import Data.Either
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Debug.Trace
import Data.Maybe
import Control.Exception
import Data.List
import Control.Monad
import Data.Function (on)

type Length = Int
type TypeInfo = [(String, [(HsExp,Bool)])]
type StateD = (Either String (HsExp,Maybe Axiom), [HsExp], TypeInfo)

showTypeInfo :: TypeInfo -> String
showTypeInfo [] = ""
showTypeInfo (t:ts) = showti t ++ "\n" ++ showTypeInfo ts
showti (t,xs) = t ++ " = " ++ (concat $ intersperse ", " $ map (\(x,y) -> showExp x ++ ":" ++ show y) xs)

showState :: StateD -> String
showState (Left s, _, _) = s
showState (Right (exp,Nothing), _, _) = showExp exp
showState (Right (exp,Just d), _, _) = showExp exp -- ++ " using " ++ showAxiom d

-- check if the answer is the same as given, using Astar search
findAnswerDecl :: [Concept] -> [Axiom] -> [Axiom] -> Int -> Int -> TypeInfo -> (Lhs,Rhs) -> IO (TypeInfo,Bool,Length)
findAnswerDecl concepts langAxioms ltm width depth t (exp,rhs) = do
            (t',ans, len) <- solveAnswer (fromIntegral width) (fromIntegral depth) concepts langAxioms ltm (Right (exp,Nothing), [], t) (Just rhs)
            return (t',(isJust ans && equalExp (fromJust ans,rhs)), len)

-- find the solution (all proof steps) using the Astar search
findSolDecl :: [Concept] -> [Axiom] -> [Axiom] -> Int -> Int -> TypeInfo -> (Lhs,Rhs) -> IO (TypeInfo,[StateD])
-- if rhs is x, then search for any answer
findSolDecl concepts langaxioms ltm width depth t (exp,(HsVar (UnQual (HsIdent "x")))) = do
        ans <- solve (fromIntegral width) (fromIntegral depth) concepts langaxioms ltm (Right (exp,Nothing), [], t) Nothing
        if ans /= Nothing
        then return (t, (Right (exp,Nothing), [], []) : fromJust ans)
        else return (t, [(Left "No solution found", [], [])])
-- else search for the given answer
findSolDecl concepts langaxioms ltm width depth t (exp,rhs) = do
        ans <- solve (fromIntegral width) (fromIntegral depth) concepts langaxioms ltm (Right (exp,Nothing), [], t) (Just rhs)
        if ans /= Nothing
        then do
            let ans' = fromJust ans
            if null ans'
            then return (t, (Right (exp,Nothing), [], t) : fromJust ans)
            else do
                let t' = head [x | (_,_,x) <- ans']
                return (t', (Right (exp,Nothing), [], t') : fromJust ans)
        else return (t,[(Left "No solution found", [], [])])

solveAnswer :: Int -> Int -> [Concept]  
                -> [Axiom] -> [Axiom] -> StateD
                -> Maybe HsExp -> IO (TypeInfo, Maybe HsExp, Length)
solveAnswer width depth concepts langAxioms mod s@(_,_,t) end = do
    result <- aStarM (expandNode concepts langAxioms mod width depth)
                        stateDist hDist (isGoal end) (return s)
    if isNothing result
    then return (t,Nothing, 0)
    else do
    let result' = fromJust result
    if null result'
    then return (t,Nothing, 0)
    else do
    let len = length result'
    let (ans, _, t1) = last result'
    let newt = foldl (\x y -> mergeTypeInfo x y) t1 [t' | (_, _, t') <- init (fromJust result)]
    return (newt, getAnswer ans, len)

solve :: Int -> Int -> [Concept] -> [Axiom] -> [Axiom] -> StateD -> (Maybe HsExp) -> IO ((Maybe [StateD]))
solve width depth concepts l mod start end = do
    result <- aStarM (expandNode concepts l mod width depth)
                        stateDist hDist (isGoal end) (return start)
    return result

getAnswer (Left _) = Nothing
getAnswer (Right (r,_)) = Just r

varBindings :: Axiom -> [(HsQName,HsExp)]
varBindings (DArrow _ (HsVar q) exp) = [(q,exp)]
varBindings (SArrow _ exp (HsVar q)) = [(q,exp)]
varBindings _ = []

isGoal :: Maybe HsExp -> StateD -> IO Bool
isGoal _ (Left _, _, _) = return True
isGoal (Just exp) (Right (r,_), _, _) = do
    --putStrLn $ show r ++ "\n" ++ show exp ++ "\n" ++ show (equalExp (r, exp)) ++ "\n\n"
    --putStrLn $ P.prettyPrint r ++ "==" ++ P.prettyPrint exp ++ " = " ++ show (equalExp (r, exp)) ++ "\n\n"
    return $ equalExp (r, exp)
isGoal Nothing (Right (r,_), _, _) = do
        let g = isSolved r
        return g

stateDist ::  StateD -> StateD -> IO Int
stateDist _ _ = return 1

hDist :: StateD -> IO Int
hDist _ = return 1

expandNode :: [Concept] -> [Axiom] -> [Axiom] -> Int -> Int -> StateD -> IO (Set StateD)
expandNode _ _ m _ maxLength (_, prev, t) | length prev >= maxLength
    = return (Set.empty)
expandNode concepts lx m maxSize maxLength (Right (exp,_), prev, t) = do
    (t1,r') <- interpreter True lx m maxSize maxLength t exp
    --putStrLn $ "\nExp: " ++ showExp exp ++ "\n"
    --putStrLn $ "New wm: " ++  show (length r')
    --putStrLn $ unlines $ map ("   " ++ ) $ map showExp $ map fst $ nub [r |Right r <- r']
    let prev' = (exp:prev)
    let right' = [Right (x,d)
                  |   (Right (x,d)) <- r', 
                      wmSize concepts x <= maxSize, 
                      not (x `elem` prev'),
                      all (\s -> not $ s `elem` prev') (getSubExp x)
                      ]
    let right = map Right $ nubBy ((==) `on` fst) [(x,d) | Right (x,d) <- right']
    let left = [Left x | Left x <- r']
    if not (null lx) && not (null right)
      then do
        return ()
        --appendFile "temp.txt" $ "\nExp: " ++ showExp exp ++ "\n"
        --appendFile "temp.txt" $ unlines $ ["    " ++ showExp r | Right (r,_) <- right]
      else return ()
    if null right
    then return (Set.fromList $ zip3 left (repeat prev') (repeat t1))
    else do
    --mapM (putStrLn . pretty) $ [r | (Right r) <- right]
    --putStrLn $ "\n\n"
    return (Set.fromList $ zip3 right (repeat prev') (repeat t1))
expandNode _ _ _ _ _ (Left l, _, t)
    = return (Set.empty)

-- | Interprets any haskell expression from the given module
-- Only produces a single step
-- if argument indicates if it is the first level (False if in recursion mode)
interpreter :: Bool -> [Axiom] -> [Axiom]
                -> Int -> Int -> TypeInfo -> HsExp
                -> IO (TypeInfo,[Either String (HsExp,Maybe Axiom)])
-- Literal
interpreter b lx axioms width depth types e@(HsLit (HsString s)) = do
        let result1 = map (\x -> Right (x,Nothing)) 
                          [e, HsList (map (\c -> HsLit (HsChar c)) s)]
        (t,result2') <- foldM (\(t,es) x -> do
                                (t1,r) <- applyRule b lx x width depth t e
                                return (t1,r++es)) (types,[]) axioms
        let result2 = map Right $ nubBy ((==) `on` fst) result2'
        return (t, result1 ++ result2)
interpreter b lx axioms width depth types e@(HsLit c) = do
        (t',exp') <- foldM (\(t,es) x -> do
                                (t1,r) <- applyRule b lx x width depth t e
                                return (t1,r++es)) (types,[]) axioms
        let exp = map Right $ nubBy ((==) `on` fst) exp'
        return (t',exp)

-- Parenthesized expression
interpreter _ lx m width depth types (HsParen exp) = do
            (t1,r) <- interpreter False lx m width depth types exp
            return (t1,[Right (HsParen e,d) | Right (e,d) <- r] ++ r)

-- Negative expression
interpreter _ lx m width depth types (HsNegApp exp) = do 
            (t1,r) <- interpreter False lx m width depth types exp
            return (t1,[Right (HsNegApp e,d) | Right (e,d) <- r])

-- Tuple of expressions
interpreter _ lx m width depth types (HsCon (Special HsUnitCon)) 
        = return (types, [Right (HsTuple [], Nothing)])
interpreter b lx axioms width depth types e@(HsCon con) = do
        (t',exp') <- foldM (\(t,es) x -> do 
                                (t1,r) <- applyRule b lx x width depth t e
                                return (t1,r++es)) (types,[]) axioms
        let exp = map Right $ nubBy ((==) `on` fst) exp'
        return (t',exp)
interpreter _ _ _ _ _ types e@(HsTuple [])  = return (types,[Right (e,Nothing)])
interpreter _ lx m width depth types e@(HsTuple [x]) = do
            (t1,r') <- interpreter False lx m width depth types x
            let r = rights r'
            if notnull r
                then return (t1, map (\(x,d) -> Right (HsTuple [x] ,d) ) r)
                else return (t1, [Left $ head $ lefts r'])

interpreter _ lx m width depth types (HsTuple (x:xs)) = do
        (t1,x') <- interpreter False lx m width depth types x
        if notnull x' && all isLeft x'
          then return (t1,x')
          else do
            let r = [Right (HsTuple (a:xs),d) | (Right (a,d)) <- x', a /= x]
            if notnull r
            then return (t1,r)
            else do
              (t2,xs') <- interpreter False lx m width depth t1 (HsTuple xs)
              let result = [Right (HsTuple (a:xsnew),d) | (Right (HsTuple xsnew,d)) <- xs', (Right (a,_)) <- x']
              if notnull result
                then return (t2,result)
                else return (t2,xs')
              -- r <- construct m list
              --return $ map (Right . HsTuple) r

-- List of expressions
interpreter b lx axioms width depth types e@(HsList [x]) = do
            (t1,r') <- interpreter False lx axioms width depth types x
            let r = [Right (HsList [a],d) | Right (a,d) <- r']
            (t2,exp') <- foldM (\(t,es) x -> do
                                    (t',r) <- applyRule b lx x width depth t e
                                    return (t',r++es))
                               (t1,[])
                               axioms
            let exp = map Right $ nubBy ((==) `on` fst) exp' -- [Right e' | e'@(HsList (e:es)) <- exp']
            if null r && null exp
                then return (t2,[Right (e,Nothing)])
                else return (t2, r ++ exp)
interpreter b lx axioms width depth types e@(HsList (x:xs)) = do
        (t1,x') <- interpreter False lx axioms width depth types x
        let r = [Right (HsList (a:xs),d) | (Right (a,d)) <- x', a /= x]
        --putStrLn $ show x
        --putStrLn $ show (length r)
        --putStrLn $ unlines $ [show e | (Right (e,d)) <- x', e /= x]
        (t2,xs') <- interpreter False lx axioms width depth t1 (HsList xs)
        let newxs = [Right (HsList (x:xsnew),d) | (Right (HsList xsnew,d)) <- xs']
        (t3,exp') <- foldM (\(t,es) x -> do
                                (t',r) <- applyRule b lx x width depth t e
                                return (t',r++es))
                           (t2,[])
                           axioms
        let exp = map Right $ nubBy ((==) `on` fst) exp'
        let result = r ++ exp ++ newxs
        if notnull result
        then return (t3,result)
        else return (t3,[Right (e,Nothing)])
interpreter b lx axioms width depth types e@(HsList _) = do
        (t',exp') <- foldM (\(t,es) x -> do
                                (t1,r) <- applyRule b lx x width depth t e
                                return (t1,r++es))
                           (types,[])
                           axioms
        let exp = map Right $ nubBy ((==) `on` fst) exp'
        if null exp
            then return (t',[Right (e,Nothing)])
            else return (t',exp)
-- Wildcard
interpreter _ _ _ _ _ types (HsWildCard) = return (types,[Right (HsWildCard,Nothing)])

-- Variable
interpreter _ _ axioms _ _ types (HsVar q@(Qual (Module m) c)) = do
    let bind' = concatMap varBindings axioms
    let bind = filter (\(x,y) -> x == q) bind'
    if null bind
        then return (types,[]) -- [Left $ "Not in scope: '" ++ prettyPrint c ++ "'"]
    else do
        let (name,exp) = last bind
        return (types,[Right (exp,Nothing)])

interpreter _ _ axioms _ _ types (HsVar q@(UnQual c)) = do
    let bind' = concatMap varBindings axioms
    let bind = filter (\(x,y) -> x == q) bind'
    if null bind
        then return (types,[]) -- [Left $ "Not in scope: '" ++ prettyPrint c ++ "'"]
    else do
        let (name,exp) = last bind
        return (types,[Right (exp,Nothing)])

--interpreter _ _ _ _ _ types (HsVar (Qual _ _)) = return [Left "Qualified variable names not supported."]
interpreter _ _ _ _ _ types (HsVar (Special _)) = return (types,[Left "Special variable names not supported."])
-- Function application
-- recursive application of the same function not allowed, e.g. f (f x)
--interpreter _ _ _ _ _ types func@(HsApp f arg@(HsApp g _)) | f == g = return []

interpreter b lx axioms width depth types func@(HsApp f@(HsApp _ _) arg) = do
            (t1,func') <- interpreter False lx axioms width depth types f
            (t2,arg')  <- interpreter False lx axioms width depth t1 arg
            (t3,exp') <- foldM (\(t,es) x -> do
                                    (t',r) <- applyRule b lx x width depth t func
                                    return (t',r ++ es))
                               (t2,[])
                               axioms
            let exp = nubBy ((==) `on` fst) exp'
            let fnew = map (\(anew,d) -> Right (HsApp f anew,d)) [(r,d) | (Right (r,d)) <- arg', r /= arg]
            if null (rights func')
            then if notnull fnew
                    then return (t3,fnew ++ [Right e | e <- exp])
                    else if null exp
                            then return (t3,func')
                            else return (t3,[Right e | e <- exp])
            else do
                 let result = [Right (HsApp r arg,d) | (Right (r,d)) <- func', r /= f]
                 return (t3, result ++ fnew ++ [Right e | e <- exp])
                 --return [Left $ "Function application " ++ show func' ++ " arg: " ++ show arg]
interpreter b lx axioms' width depth types func@(HsApp fname arg) = do
      axioms <- filterM (matchingAxiom lx width depth func) axioms'
      (t1,arg') <- interpreter False lx axioms' width depth types arg
      --putStrLn $ "Interpreter: " ++ (unlines $ map show axioms)
      if null axioms && null arg'
      then return (t1,[])
      else do
        -- check for increasing axioms
        --let incr = [x | x@(Right (exp,Just (DArrow lang lhs rhs@(HsApp f1 f2)))) <- arg1,
        --                if (wmSize [] lhs < wmSize [] rhs) then (f1 == fname) else False
        --           ]
        --let arg' = [x | x <- arg1, not (x `elem` incr)]
        (t2,exp') <- foldM (\(t,es) x -> do
                                (t',r) <- applyRule b lx x width depth t func
                                return (t',r ++ es))
                           (t1,[])
                           axioms
        let exp = nubBy ((==) `on` fst) exp'
        let argnew = [Right (HsApp fname r, d) | (r,d) <- rights arg', r /= arg]
        if null argnew
          then if null exp
                then return (t2,[])
                else return (t2, [Right e | e <- exp])
          else if null exp
                then return (t2,argnew)
                else return (t2,[Right e | e <- exp] ++ argnew)
-- Infix applications
interpreter b lx axioms width depth types func@(HsInfixApp e1 op@(HsQVarOp (UnQual opname)) e2) = do
    (t1,e1') <- interpreter False lx axioms width depth types e1
    (t2,e2') <- interpreter False lx axioms width depth t1 e2
    --let incr1 = [x | x@(Right (exp,Just (DArrow lang lhs rhs@(HsInfixApp f1 op1 f2)))) <- e1'',
    --                    if (wmSize [] lhs < wmSize [] rhs) then (op1 == op) else False ]
    --let e1' = [x | x <- e1'', not (x `elem` incr1)]
    --let incr2 = [x | x@(Right (exp,Just (DArrow lang lhs rhs@(HsInfixApp f1 op2 f2)))) <- e2'',
    --                    if (wmSize [] lhs < wmSize [] rhs) then (op2 == op) else False ]
    --let e2' = [x | x <- e2'', not (x `elem` incr2)]
    
    let e1new = [Right ((HsInfixApp x op e2),d) | (Right (x,d)) <- e1', x /= e1]
    let e2new = [Right ((HsInfixApp e1 op x),d) | (Right (x,d)) <- e2', x /= e2]
    let enew = e1new ++ e2new -- ++ map (\(Right x) -> Right (x,Nothing)) qual
    (t3,exps') <- foldM (\(t,es) x -> do
                            (t',r) <- applyRule b lx x width depth t func
                            return (t',r ++ es))
                       (t2,[]) 
                       axioms
    let exps = nubBy ((==) `on` fst) exps'
    --putStrLn $ "Interpreter: \n" ++ (unlines $ map (\(x,y) -> "  " ++ showExp x) exps)
    return (t3, [Right e | e <- exps] ++ enew)
    
interpreter b lx axioms width depth types func@(HsInfixApp e1 op@(HsQConOp (UnQual opname)) e2) = do
    (t1,e1') <- interpreter False lx axioms width depth types e1
    (t2,e2') <- interpreter False lx axioms width depth t1 e2
    --qual <- prelude func
    let e1new = [Right ((HsInfixApp x op e2),d) | (Right (x,d)) <- e1', x /= e1]
    let e2new = [Right ((HsInfixApp e1 op x),d) | (Right (x,d)) <- e2', x /= e2]
    let enew = e1new ++ e2new -- ++ map (\(Right x) -> Right (x,Nothing)) qual
    (t3,exps') <- foldM (\(t,es) x -> do
                            (t',r) <- applyRule b lx x width depth t func
                            return (t',r ++ es))
                        (t2,[]) 
                        axioms
    let exps = nubBy ((==) `on` fst) exps'
    return (t3, [Right e | e <- exps] ++ enew)
-- (TypeInfo,[(WM,Maybe Axiom)])
interpreter b lx m width depth types (HsInfixApp e1 op (HsParen e2)) 
            = interpreter b lx m width depth types (HsInfixApp e1 op e2)
interpreter b lx m width depth types (HsInfixApp (HsParen e1) op e2) 
            = interpreter b lx m width depth types (HsInfixApp e1 op e2)
interpreter _ _ m _ _ types (HsInfixApp c@(HsLit (HsChar e1)) (HsQConOp (Special HsCons)) (HsList [])) = do
    return (types, map (\x -> Right (x,Nothing)) [HsLit (HsString [e1]), HsList [c]])
interpreter _ _ m _ _ types (HsInfixApp (HsLit (HsChar e1)) (HsQConOp (Special HsCons)) (HsLit (HsString e2))) = do
    return (types,[Right (HsLit (HsString (e1:e2)),Nothing)])
interpreter _ _ m _ _ types (HsInfixApp e1 (HsQConOp (Special HsCons)) (HsList e2)) = do
    return (types,[Right (HsList (e1:e2),Nothing)])
interpreter _ _ m _ _ types (HsInfixApp e1 (HsQConOp opname) e2) =
    return (types,[]) -- [Left $ "Constructor operators not supported yet. " ++ prettyPrint opname]

-- interpreter _ _ _ = return []

-- Unsupported expression types
--interpreter _ _ (HsApp (HsLit _) _) = return [Left "Function name cannot be a literal."]
--interpreter _ _ (HsApp _ _) = return [Left "Unknown function type."]

-- Unimplemented expression types
--interpreter _ _ (HsCon _)            = return [Left "Data constructors not supported yet."]
--interpreter _ _ (HsInfixApp _ _ _)   = return [Left "Infix applications not supported yet."]
interpreter _ _ _ _ _ types _         = return (types,[Left "Not supported yet."])

{-
-- This takes an expression and evalutes it from prelude functions directly
prelude :: HsExp -> IO [Either String HsExp]
prelude func@(HsInfixApp (HsList e1) (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) (HsList e2))
    = catch (return [Right (HsList (e1 ++ e2))])
            empty
        where 
            empty :: IOException -> IO [Either String HsExp]
            empty _ = return []
prelude func@(HsInfixApp (HsList e1) (HsQVarOp (Qual (Module "P") (HsSymbol "\\\\"))) (HsList e2))
    = catch (return [Right (HsList (e1 \\ e2))])
            empty
        where 
            empty :: IOException -> IO [Either String HsExp]
            empty _ = return []
prelude func@(HsInfixApp (HsList e1) (HsQVarOp (Qual (Module "P") (HsIdent "union"))) (HsList e2))
    = catch (return [Right (HsList (e1 `union` e2))])
            empty
        where 
            empty :: IOException -> IO [Either String HsExp]
            empty _ = return []
prelude func@(HsInfixApp (HsList e1) (HsQVarOp (Qual (Module "P") (HsIdent "intersect"))) (HsList e2))
    = catch (return [Right (HsList (e1 `intersect` e2))])
            empty
        where 
            empty :: IOException -> IO [Either String HsExp]
            empty _ = return []

prelude func@(HsInfixApp (HsInfixApp e1 (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) e2) (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) e3)
    = return $ map Right [(HsInfixApp e1 (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) (HsInfixApp e2 (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) e3))]
prelude func@(HsInfixApp e1 (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) (HsInfixApp e2 (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) e3))
    = return $ map Right [(HsInfixApp (HsInfixApp e1 (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) e2) (HsQVarOp (Qual (Module "P") (HsSymbol "++"))) e3)]
prelude _ = return []
-}

isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False

-- | Match the function arguments, return the function body that matches, or Nothing
applyRule :: Bool -> [Axiom] -> Axiom -> Int -> Int -> TypeInfo -> WM -> IO (TypeInfo,[(WM,Maybe Axiom)])
applyRule _ lx d@(DArrow s lhs rhs) width depth t func = do
    (t1,rhs') <- applyRule' lx width depth t lhs rhs func
    return $ if rhs' /= Nothing then (t1,[(fromJust rhs',Just d)]) else (t1,[(func,Nothing)])
applyRule True lx d@(SArrow s lhs rhs) width depth t func = do
    (t1,rhs') <- applyRule' lx width depth t lhs rhs func
    return $ if rhs' /= Nothing then (t1,[(fromJust rhs',Just d)]) else (t1,[(func,Nothing)])
applyRule False lx d@(SArrow s lhs rhs) width depth t func = return (t,[(func,Nothing)])

-- | Check if the function argument matches with the given expression
applyRule' :: [Axiom] -> Int -> Int -> TypeInfo -> Lhs -> Rhs -> WM -> IO (TypeInfo,Maybe WM)
{-
applyRule' lx width depth (HsInfixApp e (HsQVarOp (UnQual n)) (HsInfixApp (HsVar (UnQual (HsIdent x))) (HsQVarOp (UnQual (HsSymbol "+"))) (HsLit (HsInt i))))
        exp
        arg@(HsInfixApp f (HsQVarOp (UnQual m)) (HsLit (HsInt j)))
    | n == m 
        = do b <- matchExpExp lx width depth e f
             if b
               then return $ Just $ replacePatExp (replacePatExp exp (HsVar (UnQual (HsIdent x))) (HsLit (HsInt (j - i)))) e f
               else return Nothing
applyRule' lx width depth (HsApp n (HsInfixApp x (HsQVarOp (UnQual (HsSymbol "+"))) (HsLit (HsInt i))))
        exp
        arg@(HsApp func (HsLit (HsInt j)))
    | n == func
        = return $ Just $ replacePatExp exp x (HsLit (HsInt (j - i)))
-}
applyRule' lx width depth t (HsApp (HsVar (UnQual n)) pat) exp (HsApp (HsVar (UnQual fname)) arg)
    | n == fname && null (sameVarBinding $ nub $ expVarBinding pat arg)
        = do (t',m) <- matchExpExp lx width depth t pat arg
             if m then return (t',Just $ replacePatExp exp pat arg) else return (t',Nothing)

applyRule' lx width depth t (HsApp e@(HsApp _ _) lp) exp (HsApp f@(HsApp _ _) arg)
    = do (t1,rhs) <- applyRule' lx width depth t e exp f
         (t2,m) <- matchExpExp lx width depth t1 lp arg
         if m && rhs /= Nothing
            then return (t2,Just (replacePatExp (fromJust rhs) lp arg))
            else return (t2,Nothing)
applyRule' lx width depth t (HsApp e@(HsApp _ _) lp) exp (HsApp f@(HsCon _) arg)
    = do (t1,rhs) <- applyRule' lx width depth t e exp f
         (t2,m) <- matchExpExp lx width depth t1 lp arg
         if (m && rhs /= Nothing)
            then return (t2,Just (replacePatExp (fromJust rhs) lp arg))
            else return (t2,Nothing)
applyRule' lx width depth t (HsApp (HsApp (HsVar (UnQual n)) p1) p2) exp (HsInfixApp e1 (HsQVarOp (UnQual opname)) e2)
    | n == opname 
      && null (sameVarBinding $ nub (expVarBinding p1 e1) ++ (expVarBinding p2 e2) )
      = do
            (t1,m1) <- matchExpExp lx width depth t p1 e1
            (t2,m2) <- matchExpExp lx width depth t1 p2 e2
            if m1 && m2
                then return (t2,Just $ replacePatExp (replacePatExp exp p1 e1) p2 e2)
                else return (t2,Nothing)
applyRule' lx width depth t (HsInfixApp p1 (HsQConOp (UnQual op)) p2) lhs (HsInfixApp e1 (HsQConOp (UnQual opname)) e2)
  | op == opname 
    && null (sameVarBinding $ nub ((expVarBinding p1 e1) ++ (expVarBinding p2 e2)) )
    = do
        (t1,m1) <- matchExpExp lx width depth t p1 e1
        (t2,m2) <- matchExpExp lx width depth t1 p2 e2
        if m1 && m2
            then return (t2,Just $ replacePatExp (replacePatExp lhs p1 e1) p2 e2)
            else return (t2,Nothing)
applyRule' lx width depth t ax@(HsInfixApp p1 (HsQVarOp (UnQual op)) p2) lhs wm@(HsInfixApp e1 (HsQVarOp (UnQual opname)) e2)
  | op == opname
    && null (sameVarBinding $ nub $ ((expVarBinding p1 e1) ++ (expVarBinding p2 e2)) )    
        = do
            (t1,m1) <- matchExpExp lx width depth t p1 e1
            (t2,m2) <- matchExpExp lx width depth t1 p2 e2
            --putStrLn $ "applyRule' matching: " ++ show (m1 && m2)
            --putStrLn $ "applyRule' axiom: " ++ showExp ax
            --putStrLn $ "applyRule' wm: " ++ showExp wm
            if m1 && m2
                then return (t2,Just $ replacePatExp (replacePatExp lhs p1 e1) p2 e2)
                else return (t2,Nothing)
applyRule' lx width depth t (HsList ps) exp (HsList es)
  | length es == length ps 
    && null (sameVarBinding $ nub $ concatMap (uncurry expVarBinding) $ zip ps es)
        = do m <- liftM (and . map snd) $ mapM (uncurry (matchExpExp lx width depth t)) (zip ps es)
             if m && not (null ps)
                then return (t,Just (foldl (\rhs (p,e) -> replacePatExp exp p e) exp $ zip ps es))
                else return (t,Nothing)
--applyRule' (HsApp n p) rhs arg | matchPatExp (expToPat p) arg
--        = Just rhs
--applyRule' (HsApp n pat) rhs arg
--        = Just $ replacePatExp rhs pat arg
applyRule' lx width depth t lhs rhs func = do
          let bind = (sameVarBinding $ nub $ expVarBinding lhs func)
          (t',m) <- matchExpExp lx width depth t lhs func
          if m && (null bind) then return (t',Just rhs) else return (t',Nothing)

-- Matches left HsExp with right HsExp
-- TypeInfo contains already type-checked expressions
-- TypeInfo is updated and returned if a new type checking computation is carried out
matchExpExp :: [Axiom] -> Int -> Int -> TypeInfo -> HsExp -> HsExp -> IO (TypeInfo,Bool)
matchExpExp []  _    _     types e@(HsVar (Qual (Module _) (HsIdent _  ))) _  
        = return (types,False)
matchExpExp lx width depth types e@(HsVar (Qual (Module typ) (HsIdent var))) arg
    | typ `lookup` types == Nothing
       = do (_,b,_) <- findAnswerDecl [] [] lx width depth types (arg,(HsVar (UnQual (HsIdent typ))))
            let types' = (typ,[(arg,b)]) : types
            --putStrLn $ "      -> " ++ showExp arg  ++ "::" ++ typ ++ ", " ++ show b
            --putStrLn $ "         " ++ show (length types')
            --putStrLn $ showTypeInfo types'
            return (types',b)
    | join (fmap (arg `lookup`) (typ `lookup` types)) == Nothing
       = do (t1,b,_) <- findAnswerDecl [] [] lx width depth types (arg,(HsVar (UnQual (HsIdent typ))))
            let types' = updateTypesExp (mergeTypeInfo types t1) typ arg b
            --putStrLn $ "      -> " ++ showExp arg  ++ "::" ++ typ ++ ", " ++ show b
            --putStrLn $ "      -> " ++ show (length types')
            --putStrLn $ showTypeInfo types'
            return (types',b)
    | otherwise
       = do let (Just s) = join $ fmap (arg `lookup`) (typ `lookup` types)
            return (types,s)
matchExpExp lx w d t (HsVar (UnQual x)) (HsVar (UnQual y))
        = return (t,(x == y))
matchExpExp lx w d t (HsCon x) (HsCon y)        = return (t,(x == y))
matchExpExp lx w d t (HsLit p) (HsLit e)        = return (t,(p == e))
matchExpExp lx w d t (HsNegApp p) (HsNegApp e)  = matchExpExp lx w d t p e
matchExpExp lx w d t (HsInfixApp p1 (HsQVarOp op1) p2) (HsInfixApp e1 (HsQVarOp op2) e2) | op1 == op2
   = do (t1,m1) <- matchExpExp lx w d t p1 e1
        (t2,m2) <- matchExpExp lx w d t1 p2 e2
        return (t2,m1 && m2)
matchExpExp lx w d t (HsInfixApp p1 (HsQConOp op1) p2) (HsInfixApp e1 (HsQConOp op2) e2) | op1 == op2
   = do (t1,m1) <- matchExpExp lx w d t p1 e1
        (t2,m2) <- matchExpExp lx w d t1 p2 e2
        return (t2,m1 && m2)
matchExpExp lx w d t (HsInfixApp p1 (HsQConOp (Special HsCons)) p2) (HsList (x:xs)) = do
        (t1,m1) <- matchExpExp lx w d t p1 x
        (t2,m2) <- matchExpExp lx w d t1 p2 (HsList xs)
        return (t2,m1 && m2)
{-
matchExpExp lx w d t (HsInfixApp p1 (HsQConOp (Special HsCons)) p2) (HsLit (HsString (x:xs))) = do
        (t1,m1) <- matchExpExp lx w d t p1 (HsLit (HsChar x)) 
        (t2,m2) <- matchExpExp lx w d t1 p2 (HsLit (HsString xs))
        return (t2,m1 && m2)
-}
matchExpExp lx w d t (HsTuple p) (HsTuple e) | length p == length e
   = do r <- mapM (\(x,y) -> matchExpExp lx w d t x y) $ zip p e
        return (t,(and $ map snd r))
matchExpExp lx w d t (HsList p) (HsList  e) | length p == length e
   = do r <- mapM (\(x,y) -> matchExpExp lx w d t x y) $ zip p e
        return (t,(and $ map snd r))
matchExpExp lx w d t (HsParen p) (HsParen e) = matchExpExp lx w d t p e
matchExpExp lx w d t (HsParen p) e           = matchExpExp lx w d t p e
matchExpExp lx w d t p           (HsParen e) = matchExpExp lx w d t p e
matchExpExp lx w d t (HsAsPat n2 p) (HsAsPat n1 e) | n1 == n2 
        = matchExpExp lx w d t p e
matchExpExp lx w d t HsWildCard _ = return (t,True)
matchExpExp lx w d t (HsIrrPat p) (HsIrrPat e) 
        = matchExpExp lx w d t p e
matchExpExp lx w d t (HsApp (HsCon p) p') (HsApp (HsCon e) e') | p == e 
        = matchExpExp lx w d t p' e'
matchExpExp lx w d t (HsApp (HsVar p) p') (HsApp (HsVar e) e') | p == e 
        = matchExpExp lx w d t p' e'
matchExpExp _ _ _ t _ _ = return (t,False)

mergeTypeInfo [] t = t
mergeTypeInfo t [] = t
mergeTypeInfo (t:ts) t2 = 
    let t' = (fst t) `lookup` t2
    in if t' == Nothing
        then mergeTypeInfo ts (t : t2)
        else [(tt,merge' (fromJust t') list) | (tt,list) <- t2,tt == fst t]
                ++ [x | x@(tt,_) <- t2,tt /= fst t]
merge' e1 e2 = nubBy ((==) `on` fst) (e1 ++ e2)

updateTypesExp types typ exp b 
    = [(t,(exp,b):list) | (t,list) <- types,t == typ]
       ++ [x | x@(t,_) <- types,t /= typ]
updateTypesTyp types typ exp b = (typ,[(exp,b)]) : types

matchingAxiom lx width depth func (DArrow _ m _) = do
        (_,m) <- matchExpExp lx width depth [] m func
        return m
matchingAxiom lx width depth func (SArrow _ m _) = do
        (_,m) <- matchExpExp lx width depth [] m func
        return m

axSize :: [Concept] -> Axiom -> Int
axSize c (DArrow _ x y) = wmSize c x + wmSize c y
axSize c (SArrow _ x y) = wmSize c x + wmSize c y

wmSize :: [Concept] -> HsExp -> Int
wmSize c e = 
  if e `elem` [x | (_,_,_,x) <- c]
  then 1
  else
    case e of
    HsVar _ -> 1
    HsCon _ -> 1
    HsLit (HsInt i) -> length $ show i
    HsLit _ -> 1
    HsInfixApp e1 op e2 -> wmSize c e1 + wmSize c e2 + 1
    HsApp e1 e2 -> wmSize c e1 + wmSize c e2
    HsNegApp e -> 1 + wmSize c e
    HsTuple [] -> 0
    HsTuple es -> sum $ map (wmSize c) es
    HsList [] -> 0
    HsList es -> sum $ map (wmSize c) es
    HsParen e -> wmSize c e
    HsWildCard -> 0
    HsIrrPat e -> wmSize c e
    _ -> error "unknown expression type in function wmSize"
