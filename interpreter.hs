{-
-- Module Interpreter:  functions to interpret haskell code
-- Author: Abdul Rahim Nizamani, ITIT, Gothenburg University, Sweden
-- Started: 2013-08-23
-- Updated: 2013-09-28
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

type StateD = (Either String (HsExp,Maybe Axiom), [HsExp])


showState :: StateD -> String
showState (Left s, _) = s
showState (Right (exp,Nothing), _) = showExp exp
showState (Right (exp,Just d), _) = showExp exp -- ++ " using " ++ showAxiom d

-- check if the answer is the same as given, using Astar search
findAnswerDecl :: [Concept] -> [Axiom] -> [Axiom] -> Int -> Int -> (Lhs,Rhs) -> IO Bool
findAnswerDecl concepts langAxioms ltm width depth (exp,rhs) = do
            ans <- solveAnswer (fromIntegral width) (fromIntegral depth) concepts langAxioms ltm (Right (exp,Nothing), []) (Just rhs)
            return (isJust ans && equalExp (fromJust ans,rhs))
--findAnswerDecl ltm width depth (HsFunBind [HsMatch loc name pats (HsUnGuardedRhs rhs) _]) = do
--            let exp = foldl (\exp p -> HsApp exp (patToExp p)) (HsVar (UnQual name)) pats
--            ans <- solveAnswer (fromIntegral width) (fromIntegral depth) ltm (Right (exp,Nothing), []) (Just rhs)
--            return (isJust ans && equalExp (fromJust ans,rhs))

-- find the solution (all proof steps) using the Astar search
findSolDecl :: [Concept] -> [Axiom] -> [Axiom] -> Int -> Int -> (Lhs,Rhs) -> IO [StateD]
-- if rhs is x, then search for any answer
findSolDecl concepts langaxioms ltm width depth (exp,(HsVar (UnQual (HsIdent "x")))) = do
        ans <- solve (fromIntegral width) (fromIntegral depth) concepts langaxioms ltm (Right (exp,Nothing), []) Nothing
        if ans /= Nothing
        then return $ (Right (exp,Nothing), []) : fromJust ans
        else return $ [(Left "No solution found", [])]
-- else search for the given answer
findSolDecl concepts langaxioms ltm width depth (exp,rhs) = do
        ans <- solve (fromIntegral width) (fromIntegral depth) concepts langaxioms ltm (Right (exp,Nothing), []) (Just rhs)
        if ans /= Nothing
        then return $ (Right (exp,Nothing), []) : fromJust ans
        else return $ [(Left "No solution found", [])]

solveAnswer :: Int -> Int -> [Concept] -> [Axiom] -> [Axiom] -> StateD -> Maybe HsExp -> IO (Maybe HsExp)
solveAnswer width depth concepts langAxioms mod start end = do
    result <- aStarM (expandNode concepts langAxioms mod width depth)
                        stateDist hDist (isGoal end) (return start)
    if isNothing result
    then return Nothing
    else do
    let result' = fromJust result
    if null result'
    then return Nothing
    else do
    let (ans, _) = last $ fromJust result
    return $ getAnswer ans

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
isGoal _ (Left _, _) = return True
isGoal (Just exp) (Right (r,_), _) = do
    --putStrLn $ show r ++ "\n" ++ show exp ++ "\n" ++ show (equalExp (r, exp)) ++ "\n\n"
    --putStrLn $ P.prettyPrint r ++ "==" ++ P.prettyPrint exp ++ " = " ++ show (equalExp (r, exp)) ++ "\n\n"
    return $ equalExp (r, exp)
isGoal Nothing (Right (r,_), _) = do
        let g = isSolved r
        return g

stateDist ::  StateD -> StateD -> IO Int
stateDist _ _ = return 1

hDist :: StateD -> IO Int
hDist _ = return 1

expandNode :: [Concept] -> [Axiom] -> [Axiom] -> Int -> Int -> StateD -> IO (Set StateD)
expandNode _ _ m _ maxLength (_, prev) | length prev >= maxLength
    = return Set.empty
expandNode concepts lx m maxSize maxLength (Right (exp,_), prev) = do
    r' <- interpreter True lx m maxSize maxLength exp
    -- putStrLn $ "\n   Exp: " ++ showExp exp ++ "\n"
    -- if showExp exp == "f (0 + 1)"
    -- then do
            -- let result = unlines $ map ("   " ++ ) $ map show $ nub [r |Right r <- r']
            -- writeFile "temp.txt" result
    -- else return ()
    --putStrLn $ "New wm: " ++  show (length r')
    --putStrLn $ unlines $ map ("   " ++ ) $ map show $ nub [r |Right r <- r']
    let prev' = (exp:prev)
    let right = [Right (x,d)
                  |   (Right (x,d)) <- r', 
                      wmSize concepts x <= maxSize, 
                      not (x `elem` prev)]
    let left = [Left x | Left x <- r']
    if null right
    then return $ Set.fromList $ zip left (repeat prev')
    else do
    --mapM (putStrLn . pretty) $ [r | (Right r) <- right]
    --putStrLn $ "\n\n"
    return $ Set.fromList $ zip right (repeat prev')
expandNode _ _ _ _ _ (Left l, _)
    = return $ Set.empty

-- | Interprets any haskell expression from the given module
-- Only produces a single step
-- if argument indicates if it is the first level (False if in recursion mode)
interpreter :: Bool -> [Axiom] -> [Axiom] -> Int -> Int -> HsExp -> IO [Either String (HsExp,Maybe Axiom)]
-- Literal
interpreter b lx axioms width depth e@(HsLit (HsString s)) = do
        let result1 = map (\x -> Right (x,Nothing)) 
                          [e, HsList (map (\c -> HsLit (HsChar c)) s)]
        result2' <- mapM (\x -> applyRule b lx x width depth e) axioms
        let result2 = map Right $ concat result2'
        return $ result1 ++ result2
interpreter b lx axioms width depth e@(HsLit c) = do
        exp' <- mapM (\x -> applyRule b lx x width depth e) $ axioms
        let exp = map Right $ nubBy ((==) `on` fst) $ concat exp'
        return exp

-- Parenthesized expression
interpreter _ lx m width depth (HsParen exp) = do 
            r <- interpreter False lx m width depth exp
            return $ [Right (HsParen e,d) | Right (e,d) <- r] ++ r

-- Negative expression
interpreter _ lx m width depth (HsNegApp exp) = do 
            r <- interpreter False lx m width depth exp
            return [Right (HsNegApp e,d) | Right (e,d) <- r]

-- Tuple of expressions
interpreter _ lx m width depth (HsCon (Special HsUnitCon)) = return [Right (HsTuple [], Nothing) ]
interpreter b lx axioms width depth e@(HsCon con) = do
        exp' <- mapM (\x -> applyRule b lx x width depth e) $ axioms
        let exp = map Right $ concat exp'
        return exp
interpreter _ _ _ _ _ e@(HsTuple [])  = return [Right (e,Nothing)]
interpreter _ lx m width depth e@(HsTuple [x]) = do
            r' <- interpreter False lx m width depth x
            let r = rights r'
            if notnull r
                then return $ map (\(x,d) -> Right (HsTuple [x] ,d) ) r
                else return $  [Left $ head $ lefts r']

interpreter _ lx m width depth (HsTuple (x:xs)) = do
        x' <- interpreter False lx m width depth x
        if notnull x' && all isLeft x'
          then return x'
          else do
            let r = [Right (HsTuple (a:xs),d) | (Right (a,d)) <- x', a /= x]
            if notnull r
            then return r
            else do
              xs' <- interpreter False lx m width depth (HsTuple xs)
              let result = [Right (HsTuple (a:xsnew),d) | (Right (HsTuple xsnew,d)) <- xs', (Right (a,_)) <- x']
              if notnull result
                then return result
                else return xs'
              -- r <- construct m list
              --return $ map (Right . HsTuple) r

-- List of expressions
interpreter b lx axioms width depth e@(HsList [x]) = do
            r' <- interpreter False lx axioms width depth x
            let r = [Right (HsList [a],d) | Right (a,d) <- r']
            exp' <- mapM (\x -> applyRule b lx x width depth e) $ axioms
            let exp = map Right $ concat exp' -- [Right e' | e'@(HsList (e:es)) <- exp']
            if null r && null exp
                then return [Right (e,Nothing)]
                else return $ r ++ exp
interpreter b lx axioms width depth e@(HsList (x:xs)) = do
        x' <- interpreter False lx axioms width depth x
        let r = [Right (HsList (a:xs),d) | (Right (a,d)) <- x', a /= x]
        --putStrLn $ show x
        --putStrLn $ show (length r)
        --putStrLn $ unlines $ [show e | (Right (e,d)) <- x', e /= x]
        xs' <- interpreter False lx axioms width depth (HsList xs)
        let newxs = [Right (HsList (x:xsnew),d) | (Right (HsList xsnew,d)) <- xs']
        
        exp' <- mapM (\x -> applyRule b lx x width depth e) $ axioms
        let exp = map Right $ concat exp' -- [Right e' | e'@(HsList (e:es)) <- exp']
        
        let result = r ++ exp ++ newxs
        if notnull result
        then return result
        else return [Right (e,Nothing)]
interpreter b lx axioms width depth e@(HsList _) = do
        exp' <- mapM (\x -> applyRule b lx x width depth e) $ axioms
        let exp = map Right $ concat exp' -- [Right e' | e'@(HsList (e:es)) <- exp']
        if null exp
            then return [Right (e,Nothing)]
            else return $ exp
-- Wildcard
interpreter _ _ _ _ _ (HsWildCard) = return [Right (HsWildCard,Nothing)]

-- Variable
interpreter _ _ axioms _ _ (HsVar q@(Qual (Module m) c)) = do
    let bind' = concatMap varBindings axioms
    let bind = filter (\(x,y) -> x == q) bind'
    if null bind
        then return [] -- [Left $ "Not in scope: '" ++ prettyPrint c ++ "'"]
    else do
        let (name,exp) = last bind
        return [Right (exp,Nothing)]

interpreter _ _ axioms _ _ (HsVar q@(UnQual c)) = do
    let bind' = concatMap varBindings axioms
    let bind = filter (\(x,y) -> x == q) bind'
    if null bind
        then return [] -- [Left $ "Not in scope: '" ++ prettyPrint c ++ "'"]
    else do
        let (name,exp) = last bind
        return [Right (exp,Nothing)]

--interpreter _ _ _ _ _ (HsVar (Qual _ _)) = return [Left "Qualified variable names not supported."]
interpreter _ _ _ _ _ (HsVar (Special _)) = return [Left "Special variable names not supported."]
-- Function application
-- recursive application of the same function not allowed, e.g. f (f x)
--interpreter _ _ _ _ _ func@(HsApp f arg@(HsApp g _)) | f == g = return []

interpreter b lx axioms width depth func@(HsApp f@(HsApp _ _) arg) = do
            func' <- interpreter False lx axioms width depth f
            arg'  <- interpreter False lx axioms width depth arg
            exp' <- mapM (\x -> applyRule b lx x width depth func) axioms
            let exp = concat exp'
            let fnew = map (\(anew,d) -> Right (HsApp f anew,d)) [(r,d) | (Right (r,d)) <- arg', r /= arg]
            if null (rights func')
            then if notnull fnew
                    then return $ fnew ++ [Right e | e <- exp]
                    else if null exp
                            then return func'
                            else return [Right e | e <- exp]
            else do
                 let result = [Right (HsApp r arg,d) | (Right (r,d)) <- func', r /= f]
                 return $ result ++ fnew ++ [Right e | e <- exp]
                 --return [Left $ "Function application " ++ show func' ++ " arg: " ++ show arg]
interpreter b lx axioms width depth func@(HsApp fname arg) = do
      bind <- filterM (matchingAxiom lx width depth func) axioms
             --   [d | d@(DArrow _ m _) <- axioms, matchExpExp m func]
             --    ++ [d | d@(SArrow _ m _) <- axioms, matchExpExp m func]
      arg' <- interpreter False lx axioms width depth arg
      --putStrLn $ "Interpreter: " ++ (unlines $ map show bind)
      if null bind && null arg'
      then return []
      else do
        exp' <- mapM (\x -> applyRule b lx x width depth func) $ bind
        let exp = concat exp'
        let argnew = [Right (HsApp fname r, d) | (r,d) <- rights arg', r /= arg]
        if null argnew
          then if null exp
                then return []
                else return $ [Right e | e <- exp]
          else if null exp
                then return argnew
                else return $ [Right e | e <- exp] ++ argnew
-- Infix applications
interpreter b lx axioms width depth func@(HsInfixApp e1 op@(HsQVarOp (UnQual opname)) e2) = do
    e1' <- interpreter False lx axioms width depth e1
    e2' <- interpreter False lx axioms width depth e2
    qual <- prelude func
    let e1new = [Right ((HsInfixApp x op e2),d) | (Right (x,d)) <- e1', x /= e1]
    let e2new = [Right ((HsInfixApp e1 op x),d) | (Right (x,d)) <- e2', x /= e2]
    let enew = e1new ++ e2new ++ map (\(Right x) -> Right (x,Nothing)) qual
    exps <- liftM (nub . concat) $ mapM (\x -> applyRule b lx x width depth func) axioms
    --putStrLn $ "Interpreter: \n" ++ (unlines $ map (\(x,y) -> "  " ++ showExp x) exps)
    return $ [Right e | e <- exps] ++ enew
    
interpreter b lx axioms width depth func@(HsInfixApp e1 op@(HsQConOp (UnQual opname)) e2) = do
    e1' <- interpreter False lx axioms width depth e1
    e2' <- interpreter False lx axioms width depth e2
    qual <- prelude func
    let e1new = [Right ((HsInfixApp x op e2),d) | (Right (x,d)) <- e1', x /= e1]
    let e2new = [Right ((HsInfixApp e1 op x),d) | (Right (x,d)) <- e2', x /= e2]
    let enew = e1new ++ e2new ++ map (\(Right x) -> Right (x,Nothing)) qual
    exps <- liftM (nub . concat) $ mapM (\x -> applyRule b lx x width depth func) axioms
    return $ [Right e | e <- exps] ++ enew

interpreter b lx m width depth (HsInfixApp e1 op (HsParen e2)) 
            = interpreter b lx m width depth (HsInfixApp e1 op e2)
interpreter b lx m width depth (HsInfixApp (HsParen e1) op e2) 
            = interpreter b lx m width depth (HsInfixApp e1 op e2)
interpreter _ _ m _ _ (HsInfixApp c@(HsLit (HsChar e1)) (HsQConOp (Special HsCons)) (HsList [])) = do
    return $ map (\x -> Right (x,Nothing)) [HsLit (HsString [e1]), HsList [c]]
interpreter _ _ m _ _ (HsInfixApp (HsLit (HsChar e1)) (HsQConOp (Special HsCons)) (HsLit (HsString e2))) = do
    return [Right (HsLit (HsString (e1:e2)),Nothing)]
interpreter _ _ m _ _ (HsInfixApp e1 (HsQConOp (Special HsCons)) (HsList e2)) = do
    return [Right (HsList (e1:e2),Nothing)]
interpreter _ _ m _ _ (HsInfixApp e1 (HsQConOp opname) e2) =
    return [] -- [Left $ "Constructor operators not supported yet. " ++ prettyPrint opname]

-- interpreter _ _ _ = return []

-- Unsupported expression types
--interpreter _ _ (HsApp (HsLit _) _) = return [Left "Function name cannot be a literal."]
--interpreter _ _ (HsApp _ _) = return [Left "Unknown function type."]

-- Unimplemented expression types
--interpreter _ _ (HsCon _)            = return [Left "Data constructors not supported yet."]
--interpreter _ _ (HsInfixApp _ _ _)   = return [Left "Infix applications not supported yet."]
interpreter _ _ _ _ _ _                   = return [Left "Not supported yet."]


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

isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False

-- | Match the function arguments, return the function body that matches, or Nothing
applyRule :: Bool -> [Axiom] -> Axiom -> Int -> Int -> WM -> IO [(WM,Maybe Axiom)]
applyRule _ lx d@(DArrow s lhs rhs) width depth func = do
    rhs' <- applyRule' lx width depth lhs rhs func
    return $ if rhs' /= Nothing then [(fromJust rhs',Just d)] else [(func,Nothing)]
applyRule True lx d@(SArrow s lhs rhs) width depth func = do
    rhs' <- applyRule' lx width depth lhs rhs func
    return $ if rhs' /= Nothing then [(fromJust rhs',Just d)] else [(func,Nothing)]
applyRule False lx d@(SArrow s lhs rhs) width depth func = return [(func,Nothing)]

-- | Check if the function argument matches with the given expression
applyRule' :: [Axiom] -> Int -> Int -> Lhs -> Rhs -> WM -> IO (Maybe WM)
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
applyRule' lx width depth (HsApp (HsVar (UnQual n)) pat) exp (HsApp (HsVar (UnQual fname)) arg)
    | n == fname && null (sameVarBinding $ nub $ expVarBinding pat arg)
        = do m <- matchExpExp lx width depth pat arg
             if m then (return $ Just $ replacePatExp exp pat arg) else return Nothing

applyRule' lx width depth (HsApp e@(HsApp _ _) lp) exp (HsApp f@(HsApp _ _) arg)
    = do rhs <- applyRule' lx width depth e exp f
         m <- matchExpExp lx width depth lp arg
         if m && rhs /= Nothing
            then return (Just (replacePatExp (fromJust rhs) lp arg))
            else return Nothing
applyRule' lx width depth (HsApp e@(HsApp _ _) lp) exp (HsApp f@(HsCon _) arg)
    = do rhs <- applyRule' lx width depth e exp f
         m <- matchExpExp lx width depth lp arg
         if (m && rhs /= Nothing)
            then return (Just (replacePatExp (fromJust rhs) lp arg))
            else return Nothing

applyRule' lx width depth (HsApp (HsApp (HsVar (UnQual n)) p1) p2) exp (HsInfixApp e1 (HsQVarOp (UnQual opname)) e2)
    | n == opname 
      && null (sameVarBinding $ nub (expVarBinding p1 e1) ++ (expVarBinding p2 e2) )
      = do
            m1 <- matchExpExp lx width depth p1 e1
            m2 <- matchExpExp lx width depth p2 e2
            if m1 && m2
                then return $ Just $ replacePatExp (replacePatExp exp p1 e1) p2 e2
                else return Nothing
applyRule' lx width depth (HsInfixApp p1 (HsQConOp (UnQual op)) p2) lhs (HsInfixApp e1 (HsQConOp (UnQual opname)) e2)
  | op == opname 
    && null (sameVarBinding $ nub ((expVarBinding p1 e1) ++ (expVarBinding p2 e2)) )
    = do
            m1 <- matchExpExp lx width depth p1 e1
            m2 <- matchExpExp lx width depth p2 e2
            if m1 && m2
                then return $ Just $ replacePatExp (replacePatExp lhs p1 e1) p2 e2
                else return Nothing
applyRule' lx width depth ax@(HsInfixApp p1 (HsQVarOp (UnQual op)) p2) lhs wm@(HsInfixApp e1 (HsQVarOp (UnQual opname)) e2)
  | op == opname
    && null (sameVarBinding $ nub $ ((expVarBinding p1 e1) ++ (expVarBinding p2 e2)) )    
        = do
            m1 <- matchExpExp lx width depth p1 e1
            m2 <- matchExpExp lx width depth p2 e2
            --putStrLn $ "applyRule' matching: " ++ show (m1 && m2)
            --putStrLn $ "applyRule' axiom: " ++ showExp ax
            --putStrLn $ "applyRule' wm: " ++ showExp wm
            if m1 && m2
                then return $ Just $ replacePatExp (replacePatExp lhs p1 e1) p2 e2
                else return Nothing
applyRule' lx width depth (HsList ps) exp (HsList es)
  | length es == length ps 
    && null (sameVarBinding $ nub $ concatMap (uncurry expVarBinding) $ zip ps es)
        = do m <- liftM and $ mapM (uncurry (matchExpExp lx width depth)) (zip ps es)
             if m && not (null ps)
                then return $ Just (foldl (\rhs (p,e) -> replacePatExp exp p e) exp $ zip ps es)
                else return Nothing
--applyRule' (HsApp n p) rhs arg | matchPatExp (expToPat p) arg
--        = Just rhs
--applyRule' (HsApp n pat) rhs arg
--        = Just $ replacePatExp rhs pat arg
applyRule' lx width depth lhs rhs func = do
          let bind = (sameVarBinding $ nub $ expVarBinding lhs func)
          m <- matchExpExp lx width depth lhs func
          if m && (null bind) then return (Just rhs) else return Nothing

-- Matches left HsExp with right HsExp
matchExpExp :: [Axiom] -> Int -> Int -> HsExp -> HsExp -> IO Bool
matchExpExp lx width depth e@(HsVar (Qual (Module m) var)) arg = do
    if null lx
        then return False
        else do
            b <- findAnswerDecl [] [] lx width depth (arg,(HsVar (UnQual (HsIdent m))))
            return b
matchExpExp lx w d (HsVar (UnQual x)) (HsVar (UnQual y)) = return (x == y)
matchExpExp lx w d (HsCon x) (HsCon y) = return (x == y)
matchExpExp lx w d (HsLit p) (HsLit e) = return (p == e)
matchExpExp lx w d (HsNegApp p) (HsNegApp e) = matchExpExp lx w d p e
--matchExpExp lx w d (HsInfixApp p1 (HsQVarOp (UnQual (HsSymbol "+"))) (HsLit (HsInt _))) (HsLit (HsInt _)) = return True
matchExpExp lx w d (HsInfixApp p1 (HsQVarOp op1) p2) (HsInfixApp e1 (HsQVarOp op2) e2) | op1 == op2
   = do m1 <- matchExpExp lx w d p1 e1
        m2 <- matchExpExp lx w d p2 e2
        return (m1 && m2)
matchExpExp lx w d (HsInfixApp p1 (HsQConOp op1) p2) (HsInfixApp e1 (HsQConOp op2) e2) | op1 == op2
   = do m1 <- matchExpExp lx w d p1 e1
        m2 <- matchExpExp lx w d p2 e2
        return (m1 && m2)
matchExpExp lx w d (HsInfixApp p1 (HsQConOp (Special HsCons)) p2) (HsList (x:xs)) = do
        m1 <- matchExpExp lx w d p1 x
        m2 <- matchExpExp lx w d p2 (HsList xs)
        return (m1 && m2)
matchExpExp lx w d (HsInfixApp p1 (HsQConOp (Special HsCons)) p2) (HsLit (HsString (x:xs))) = do
        m1 <- matchExpExp lx w d p1 (HsLit (HsChar x)) 
        m2 <- matchExpExp lx w d p2 (HsLit (HsString xs))
        return (m1 && m2)
matchExpExp lx w d (HsTuple p) (HsTuple e) | length p == length e
   = do r <- mapM (\(x,y) -> matchExpExp lx w d x y) $ zip p e
        return (and r)
matchExpExp lx w d (HsList p) (HsList  e) | length p == length e
   = do r <- mapM (\(x,y) -> matchExpExp lx w d x y) $ zip p e
        return (and r)
matchExpExp lx w d (HsParen p) (HsParen e) = matchExpExp lx w d p e
matchExpExp lx w d (HsParen p) e           = matchExpExp lx w d p e
matchExpExp lx w d p            (HsParen e) = matchExpExp lx w d p e
matchExpExp lx w d (HsAsPat n2 p) (HsAsPat n1 e) | n1 == n2 = matchExpExp lx w d p e
matchExpExp lx w d HsWildCard _ = return True
matchExpExp lx w d (HsIrrPat p) (HsIrrPat e) = matchExpExp lx w d p e
matchExpExp lx w d (HsCon p) (HsCon e) = return (p == e)
matchExpExp lx w d (HsApp (HsCon p) p') (HsApp (HsCon e) e') | p == e = matchExpExp lx w d p' e'
matchExpExp lx w d (HsApp (HsVar p) p') (HsApp (HsVar e) e') | p == e = matchExpExp lx w d p' e'
matchExpExp _ _ _ _ _ = return False

matchingAxiom lx width depth func (DArrow _ m _) = matchExpExp lx width depth m func
matchingAxiom lx width depth func (SArrow _ m _) = matchExpExp lx width depth m func

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
