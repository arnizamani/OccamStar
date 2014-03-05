{-
    OccamPlus: symbolic reasoner and learner
    Author: Abdul Rahim Nizamani, ITIT, Gothenburg University, Sweden
    Started: 2013-09-29
    Updated: 2014-03-04
-}

module Main where
import System.Environment (getArgs, getProgName)
import Niz
import Parsing
import Data.Char
import Instances
import Interpreter
import Data.List
import Data.Word
import Data.Maybe
import Language.Haskell.Syntax
import Language.Haskell.Parser
import qualified Language.Haskell.Pretty as P
import System.IO
import Haskell
import Data.Maybe (isNothing)
import Control.Monad (foldM)
import Control.Parallel.Strategies
import Debug.Trace

type Delta = [Axiom]
noSrc = SrcLoc "" 0 0

printMessage = do
    p <- getProgName
    putStrLn $ "Usage: " ++ p ++ " agent.hs"
    putStrLn $ "       where: agent.hs contains the agent description and memory"

-- | Save an agent in a file
saveAgent :: Agent -> FilePath -> IO ()
saveAgent (Agent comments (width,depth,sol) (cfile,file) (axioms,concepts)) filename = do
    writeFile filename comments
    appendFile filename $ "-}\n"
    appendFile filename $ unlines $ map showAxiom axioms
    appendFile filename $ "\n"
    writeFile cfile $ unlines $ map showConcept concepts

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
    then printMessage
    else do
    let [agentfile] = take 1 args
    agent' <- parseAgent agentfile
    if isNothing agent'
    then do
        putStrLn $ "Error reading agent."
        return ()
    else do
    let (Just agent@(Agent c (width,depth,sol) (cfile,file) (axioms,concepts))) = agent'
    (pos,neg) <- parseTrainingFile file
    let examples = pos ++ neg
    let units = makeUnitConcepts examples
    let unary = makeUnaryConcepts examples
    let binary = makeBinaryConcepts examples
    let newc = units ++ unary ++ binary
    let t = if null newc then concepts else foldl (flip insertInConcepts) [] (concepts ++ newc)
    let newAgent = (Agent c (width,depth,sol) (cfile,file) (axioms,t))
    -- putStrLn $ show $ wmSize concepts $ lhs $ head pos
    expnew <- findDelta 0 newAgent neg pos
    --expnew <- trainAgent agent
    if null (fst expnew)
    then do
        --putStrLn $ "All examples solved. Nothing to learn."
        saveAgent newAgent agentfile
    else do
        putStrLn $ "\nLearned this rule: "
        putStrLn . unlines . map showAxiom $ fst expnew
        putStrLn . show $ snd expnew
        putStrLn $ "Do you want the agent to remember it? (Y/n) "
        c <- getChar
        if c == 'n' || c == 'N'
        then  saveAgent newAgent agentfile
        else do
            let (Agent c (width,depth,sol) (cfile,file) (axioms,concepts)) = newAgent
            let newltm = union axioms (fst expnew)
            let newAgent' = Agent c (width,depth,sol) (cfile,file) (newltm,concepts)
            saveAgent newAgent' agentfile
            putStrLn $ "Stored in memory."
        
findDelta :: Int -> Agent -> [IP] -> [IP] -> IO ([Axiom],Int)
findDelta len ag _ _ | len < 0 = return ([],0)
findDelta len agent neg posex = do
    let (Agent c (width,depth,sol) (cfile,file) (ltm',concepts)) = agent
    let langs = [lang |  (IP lang _ _ _) <- posex]
    let pos = [x  |  x@(IP _ p e v) <- posex,
                     e /= HsVar (UnQual (HsIdent "x"))]
    let ded = [x  |  x@(IP _ p (HsVar (UnQual (HsIdent "x"))) v) <- posex]
    let posAxioms = [DArrow s p e | IP s p e v <- pos]
    let optimum   = sum ([v | IP _ p e v <- pos, v > 0]
                          ++ [v | IP _ p e v <- ded, v > 0])
    if optimum < 1
    then return ([],0)
    else do
    
    if len > sum (map size pos)
    then do putStrLn $ "Size exceeded from " ++ show (sum (map size pos))
            return (posAxioms,sum [v | (IP _ _ _ v) <- pos, v > 0])
    else do
    if len > fromIntegral sol
    then do putStrLn $ "Maximum size reached: " ++ show sol
            return ([],0)
    else do
    let tag = head $ map getTag posex
    let langAxioms = if tag == "Lang"
                        then []
                        else [x | x@(DArrow "Lang" _ _) <- ltm']
                     -- [x | x@(DArrow _ _ (HsVar (UnQual _))) <- ltm']
    let ltm = [x | x@(DArrow s _ _) <- ltm', s == tag]
              ++ [x | x@(SArrow s _ _) <- ltm', s == tag]
    if len < 1
    then do
        ans <- mapM (\(IP _ x y _) -> 
                findAnswerDecl concepts langAxioms ltm width depth (x,y)) pos
        if and ans && not (null ans)
        then do
            newans <- mapM (\(IP _ x y _) -> 
                        findSolDecl concepts langAxioms ltm width depth (x,y)) pos
            putStrLn $ "Computations1: "
            putStrLn $ unlines $ map (\x -> unlines $ map showState x) newans
            putStrLn $ "All examples solved. Nothing to learn.\n"
            return ([],0)
        else do
            newans <- mapM (\(IP _ x y _) -> findSolDecl concepts langAxioms ltm width depth (x,y)) ded
            let lefts = [x | x@(Left _) <- (map fst (map head newans))]
            if null lefts && (not . null) ded
            then do
                putStrLn $ "Computations2: "
                putStrLn $ unlines $ map (\x -> unlines $ map showState x) newans
                putStrLn $ "All examples solved. Nothing to learn.\n"
                return ([],0)
            else findDelta 1 agent neg pos
    else do
        putStrLn $ "Searching at length: " ++ show len
        let funcs = [ (i, [f | f <- generateFuncsAll lang concepts i pos,
                               isPure f,
                               not (f `elem` ltm),
                               if tag=="Lang" then rhsIsVar f else True
                           ])
                          | i <- [1..len], 
                            lang <- langs]
        let delta2 = concat $
                        [ [[g1,g2],[g2,g1]]
                                | i <- [1..(len `div` 2)],
                                  g1 <- fromJust (lookup i funcs),
                                  g2 <- fromJust (lookup (len-i) funcs),
                                  not (numberChange g1),
                                  not (numberChange g2),
                                  if i == (len-i) then g1 /= g2 else True,
                                  not (lhsNotSame g1 g2)
                            ]
        let delta1 =     [ [g]  | g <- fromJust (lookup len funcs),
                                  not (numberChange g)
                          ]
        
        putStrLn $ "    generated functions: " ++ show (length delta1 + length delta2)
        --appendFile "temp.txt"
        --  $ unlines $ map (\x -> concat . intersperse ", " $ map showAxiom x) delta
        
        let func delta = do
                result <- sequence $ parMap rpar (performance width depth sol concepts langAxioms ltm neg pos ded) delta
                let result' = [(x,y) | (Just x,y) <- result]
                let optimal  = [(x,y) | (x,y) <- result', y == optimum]
                let best = maximum $ map snd result'
                let optimal' = [(x,y) | (x,y) <- result', y == best]
                if (not . null) optimal
                then do
                    putStrLn $ "Optimal performance: " ++ show optimum
                    putStrLn $ show (length optimal) ++ " optimal deltas found."
                    let delta = chooseBestDelta optimal
                    return $ Just delta
                else do
                    if len == fromIntegral sol && (not . null) optimal'
                    then do putStrLn $ "Best performance: " ++ show optimum
                            putStrLn $ show (length optimal) ++ " best deltas found."
                            let delta = chooseBestDelta optimal'
                            return $ Just delta
                    else return Nothing

        result2 <- func delta2
        if result2 /= Nothing
        then return $ fromJust result2
        else do
            result1 <- func delta1
            if result1 /= Nothing
            then return $ fromJust result1
            else findDelta (len+1) agent neg pos

numberChange (DArrow _ (HsLit (HsInt _)) _) = True
numberChange (SArrow _ (HsLit (HsInt _)) _) = True
numberChange _ = False

-- for Language, the rhs must be a string
rhsIsVar (DArrow _ p (HsVar (UnQual _))) = True
rhsIsVar (DArrow _ p (HsLit (HsString _))) = True
rhsIsVar _ = False

isPure (DArrow _ p q) = 
    let pvar = nub [HsVar e  |(HsVar e@(Qual _ _)) <- getSubExp p]
        qvar = nub [HsVar e  |(HsVar e@(Qual _ _)) <- getSubExp q]
    in null (qvar \\ pvar)
isPure (SArrow _ p q) = 
    let pvar = nub [HsVar e  |(HsVar e@(Qual _ _)) <- getSubExp p]
        qvar = nub [HsVar e  |(HsVar e@(Qual _ _)) <- getSubExp q]
    in null (qvar \\ pvar)
lhsNotSame (DArrow s (HsVar _) y) (DArrow t (HsVar _) q) = s == t
lhsNotSame (DArrow s (HsVar _) y) (SArrow t (HsVar _) q) = s == t
lhsNotSame (SArrow s (HsVar _) y) (SArrow t (HsVar _) q) = s == t
lhsNotSame (SArrow s (HsVar _) y) (DArrow t (HsVar _) q) = s == t
lhsNotSame (DArrow s x y) (DArrow t p q) = x == p && s == t
lhsNotSame (SArrow s x y) (SArrow t p q) = x == p && s == t
lhsNotSame _ _ = False
-----------------------

chooseBestDelta :: [([Axiom],Int)] -> ([Axiom],Int)
chooseBestDelta [] = ([],0)
chooseBestDelta [x] = x
chooseBestDelta deltas =
    let deltas' = [(ax,perf,length [p | p@(DArrow _ _ _) <- ax]) | (ax,perf)  <- deltas]
        maxArrowCount = maximum [arrows | (ax,perf,arrows) <- deltas']
        deltas1 = [(ax,perf) | x@(ax,perf,arrows) <- deltas', arrows == maxArrowCount]
    in if length deltas1 == 1
        then head deltas1
        else let deltas2 = [(ax,perf,sum (map countVars ax)) | (ax,perf)  <- deltas1]
                 maxVarCount = maximum [varCount | (ax,perf,varCount) <- deltas2]
                 deltas3 = [(ax,perf) | x@(ax,perf,vars) <- deltas2, vars == maxVarCount]
             in if length deltas3 == 1
                  then head deltas3
                  else head deltas3

countVars :: Axiom -> Int
countVars (DArrow _ p q) = countVars' p + countVars' q
countVars (SArrow _ p q) = countVars' p + countVars' q
countVars' exp = length [HsVar e  |(HsVar e) <- getSubExp exp]

performance :: Int -> Int -> Int 
            -> [Concept] -> [Axiom] -> [Axiom]
            -> [IP] -> [IP] -> [IP]
            -> Delta -> IO (Maybe Delta,Int)
performance width depth sol concepts laxioms ltm negex pos ded func = do
    --putStrLn $ unlines $ map showAxiom func
    let ltm' = ltm ++ func
    ansPos <- mapM (\(IP _ a b y) -> do 
                result <- findAnswerDecl concepts laxioms ltm' width depth (a,b)
                return (result,y)) pos
    ansDed <- mapM (\(IP _ a b y) -> do 
                result <- findSolDecl concepts laxioms ltm' width depth (a,b)
                return (result,y)) ded
    ansNeg <- mapM (\(IP _ a b y) -> do 
                result <- findAnswerDecl concepts laxioms ltm' width depth (a,b)
                return (result,y)) negex
    let posUtil = sum [y | (True,y) <- ansPos]
    let dedUtil = sum [y | (sol,y) <- ansDed,
                           (Right _,_) <- [head sol]]
    let negUtil = sum [y | (True,y) <- ansNeg]
    let util = posUtil + dedUtil - negUtil
    if or (map fst ansNeg)
    then return (Nothing,util)
    else do
        if and (map fst ansPos)
        then do
            if null [x | (x@(Left _),_) <- (map head (map fst ansDed))]
            then return (Just func,util)
            else return (Nothing,util)
        else return (Nothing,util)

generateFuncsAll :: Language -> [Concept] -> Int -> [IP] -> Delta
generateFuncsAll _  _  len _   | len < 2 = []
generateFuncsAll lang concepts len pos =
    let variables = if lang == "Lang" then "" else "x"
        domains = [s | (_,_,f,(HsVar (UnQual (HsIdent s)))) <- concepts, f > 1]
        parts = [lhs | (IP _ lhs _ _) <- pos] ++ [rhs | (IP _ _ rhs _) <- pos]
        units = nub $ [x | x <- (concatMap getSubExp parts), size x == 1]
                      ++ [HsVar (Qual (Module d) (HsIdent [c]))
                                | c <- variables, d <- domains]
                      ++ [exp | (l,arit,freq,exp) <- concepts, 
                                arit==0,
                                freq > 1,
                                l==lang]
                                -- size exp == 1]
        unary  = [exp | (l,arit,freq,exp) <- concepts, arit==1, l==lang] -- ,size exp == 1]
                    -- ++ [HsVar x | (HsApp (HsVar x) _) <- parts]
        binary = [exp | (l,arit,freq,exp) <- concepts, arit==2, l==lang] -- ,size exp == 1]
                    -- ++ [HsVar x | (HsInfixApp _ (HsQVarOp x) _) <- parts]
        exps = [ (i, generateExps i units unary binary) | i <- [1..(len-1)] ]
        result = concat
                 [ [DArrow lang p q, DArrow lang q p]
                      | i <- [1 .. (len `div` 2)],
                        p <- fromJust (lookup i exps),
                        q <- fromJust (lookup (len - i) exps),
                        if i == (len-i) then p /= q else True
                  ]
    in  result -- ++ [x :-> y | (x :->> y) <- result]

generateExps :: Int -> [HsExp] -> [HsExp] -> [HsExp] -> [HsExp]
generateExps len _ _ _ | len < 1 = []
generateExps 1 units _ _ = units
generateExps 2 units funcs1 _ = [HsApp x y | x <- funcs1, y <- units]
generateExps len units funcs1 funcs2
    = let exps2 = generateExps (len-2) units funcs1 funcs2
          exps1 = generateExps (len-1) units funcs1 funcs2
      in [HsInfixApp x (HsQVarOp op) y
            | x <- exps2,
              y <- exps2,
              (HsVar op) <- funcs2]
         ++ [HsApp op arg
              | op <- funcs1,
                arg <- exps1]

