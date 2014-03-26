{-
    OccamStar: symbolic reasoner and learner
    Author: Abdul Rahim Nizamani, ITIT, Gothenburg University, Sweden
    Started: 2013-09-29
    Updated: 2014-03-06
-}

module Main where
import System.Environment (getArgs, getProgName)
import Niz
import Parsing
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Instances
import Interpreter
import Data.List
import Data.Word
import Data.Maybe
import Language.Haskell.Syntax
import Language.Haskell.Parser
import qualified Language.Haskell.Pretty as P
import System.IO
import System.Directory (doesFileExist)
import Haskell
import Data.Maybe (isNothing)
import Data.Function (on)
import Control.Arrow
import Control.Monad (foldM)
import Control.Parallel.Strategies
import Debug.Trace

type Concepts = Set HsExp
type Utility = Int
type Delta = [Axiom]
noSrc = SrcLoc "" 0 0

-- | Free variables are those that occur in lhs but not in rhs.
-- | For example, the axiom (x*0=0) has one free variable.
freeVariables = 1

-- | Use abstraction (anti-unification) when d > 6
useAbstraction = True

printMessage = do
    p <- getProgName
    putStrLn $ "Usage: "
    putStrLn $ "To create a new agent: "
    putStrLn $ "       " ++ p ++ " -create AgentName"
    putStrLn $ "To run an existing agent: "
    putStrLn $ "       " ++ p ++ " -run AgentFile IPfile"
    putStrLn $ "    where: AgentFile contains the agent description and memory"
    putStrLn $ "           IPfile contains the examples to learn from"

-- | Saves an agent in a file
saveAgent :: Agent -> FilePath -> IO ()
saveAgent (Agent comments (width,depth,sol) (axioms,concepts)) filename = do
    writeFile filename comments
    appendFile filename $ "-}\n"
    appendFile filename $ unlines $ map showAxiom axioms
    appendFile filename $ unlines $ map showConcept concepts

createAgent :: String -> IO ()
createAgent name = do
    exist <- doesFileExist name
    if exist
    then putStrLn $ "Error: file " ++ name ++ " already exists."
    else do
    writeFile name $ unlines
        $["{-",
          "    Occam Agent: " ++ name,
          "-}",
          "Width ->>_Param 8",
          "Depth ->>_Param 10",
          "Solution ->>_Param 6"]
    putStrLn $ "Created agent " ++ name

main :: IO ()
main = do
    args' <- getArgs
    if null args'
    then printMessage
    else do
    if (take 1 $ head args') /= "-"
    then printMessage
    else do
    let ((_:command):args) = args'
    case command of
     "create" -> if null args then printMessage else createAgent (head args)
     "run" -> do
        if length args < 2
        then printMessage
        else do
        let [agentfile,ipfile] = take 2 args
        agent' <- parseAgent agentfile
        if isNothing agent'
        then do
            putStrLn $ "Error reading agent."
            return ()
        else do
        writeFile "trace.txt" " \n"
        let (Just agent@(Agent comm param@(width,depth,sol) (axioms,concepts))) = agent'
        (pos,neg) <- parseTrainingFile ipfile
        let examples = pos ++ neg
        let units = makeUnitConcepts examples
        let unary = makeUnaryConcepts examples
        let binary = makeBinaryConcepts examples
        let ipConcepts = nub (units ++ unary ++ binary)
        let updatedConcepts = foldl (flip insertInConcepts) [] (concepts ++ ipConcepts)
        (delt,util,len) <- findDelta 0 (Agent comm param (axioms,updatedConcepts)) neg pos
        if not (null delt)
        then do
            putStrLn $ "\nLearned this rule: "
            putStrLn . unlines . map showAxiom $ delt
            putStrLn . show $ util
            putStrLn $ "Do you want the agent to remember it? (Y/n) "
            c <- getChar
            if c == 'n' || c == 'N'
            then  saveAgent (Agent comm param (axioms,updatedConcepts)) agentfile
            else do -- update productions
                saveAgent (Agent comm param ((union axioms delt),updatedConcepts)) agentfile
                putStrLn $ "Stored in memory."
        else do
            saveAgent (Agent comm param (axioms,updatedConcepts)) agentfile
     _  -> printMessage
    
        
findDelta :: Int -> Agent -> [IP] -> [IP] -> IO ([Axiom],Utility,Length)
findDelta len ag _ _ | len < 0 = return ([],0,0)
findDelta len agent neg posex = do
    let (Agent c (width,depth,sol) (ltm',concepts)) = agent
    let langs = [lang |  (IP lang _ _ _) <- posex]
    let pos = [x  |  x@(IP _ p e v) <- posex,
                     e /= HsVar (UnQual (HsIdent "x"))]
    let ded = [x  |  x@(IP _ p (HsVar (UnQual (HsIdent "x"))) v) <- posex]
    let posAxioms = [DArrow s p e | IP s p e v <- pos, v > 0]
    let posDelta = (posAxioms,sum [v | (IP _ _ _ v) <- pos, v > 0],0)
    let optimum   = sum ([v | IP _ p e v <- pos, v > 0]
                          ++ [v | IP _ p e v <- ded, v > 0])
    if optimum < 1
    then return posDelta
    else do
    if len > sum (map size pos)
    then do
        putStrLn $ "Size exceeded from " ++ show (sum (map size pos))
        return posDelta
    else do
    if len > fromIntegral sol
    then do 
        putStrLn $ "Maximum size reached: " ++ show sol
        return posDelta
    else do
    let tag = head $ map getTag posex
    let langAxioms = if tag == "Lang"
                        then []
                        else [x | x@(DArrow "Lang" _ _) <- ltm']
                     -- [x | x@(DArrow _ _ (HsVar (UnQual _))) <- ltm']
    let ltm = [x | x@(DArrow s _ _) <- ltm', s == tag]
              ++ [x | x@(SArrow s _ _) <- ltm', s == tag]
    let ti = [] :: TypeInfo
    if len < 1
    then do
        (t1,ans) <- foldM (\(t,xs) (IP _ x y _) -> do
                            (t',b,_) <- findAnswerDecl concepts langAxioms ltm width depth t (x,y)
                            return (t',b:xs)) (ti,[]) pos
        if and ans && not (null ans)
        then do
            (t2,newans) <- foldM (\(t,xs) (IP _ x y _) -> do
                            (t',r) <- findSolDecl concepts langAxioms ltm width depth t (x,y)
                            return (t',r:xs)) (t1,[]) pos
            putStrLn $ "Computations1: "
            putStrLn $ unlines $ map (\x -> unlines $ map showState x) newans
            putStrLn $ "All examples solved. Nothing to learn.\n"
            return ([],0,1)
        else do
            (t2,newans) <- foldM (\(t,xs) (IP _ x y _) -> do
                                (t',r) <- findSolDecl concepts langAxioms ltm width depth t (x,y)
                                return (t,r:xs)) (t1,[]) ded
            let lefts = [x | x@(Left _) <- [x | (x,_,_) <- (map head newans)]]
            if null lefts && (not . null) ded
            then do
                putStrLn $ "Computations2: "
                putStrLn $ unlines $ map (\x -> unlines $ map showState x) newans
                putStrLn $ "All examples solved. Nothing to learn.\n"
                return ([],0,1)
            else findDelta 1 agent neg pos
    else do
        putStrLn $ "Searching at length: " ++ show len
        let posrhs = [e | IP s p e v <- pos, not (containsVar e)]
        let funcs' = [ (i, [f | f <- generateFuncsAll lang concepts i pos,
                               isPure f,
                               axSize [] f == i,
                               not (f `elem` ltm),
                               not ((axLhs f) `elem` posrhs),
                               length (freeVars f) <= freeVariables,
                               singleDomain f,
                               not (lhsIsVar f),
                               wmSize [] (axLhs f) >= wmSize [] (axRhs f),
                               if tag=="Lang" then rhsIsTag f else True
                           ])
                          | i <- [1..len], 
                            lang <- langs]
        let funcs = map (second (nubBy (\x y -> lhsIsSame x y && axRhs x == axRhs y)))
                    funcs'
        let delta2 = concat $
                        [ [[g1,g2]]
                                | i <- [1..(len `div` 2)],
                                  g1 <- fromJust (lookup i funcs),
                                  g2 <- fromJust (lookup (len-i) funcs),
                                  not (numberChange g1),
                                  not (numberChange g2),
                                  if i == (len-i) then g1 /= g2 else True,
                                  not (lhsIsSame g1 g2)
                            ]
        let delta1 =     [ [g]  | g <- fromJust (lookup len funcs),
                                  not (numberChange g)
                          ]
        let delta12 = delta2 ++ (if len < 6 then delta1 else [])
        --let delta1 = nubBy (\(x:[]) (y:[]) -> lhsIsSame x y && axRhs x == axRhs y) delta1''
        putStrLn $ "    generated functions: " ++ show (length delta12)
        --appendFile "temp.txt"
        --  $ unlines $ map (\x -> concat . intersperse ", " $ map showAxiom x) delta
        -- let ti = [] :: TypeInfo
        let occam delta = do
                (t2,result) <- foldM (\(t,xs) d -> do
                                (t',r) <- performance width depth sol concepts langAxioms ltm t neg pos ded d
                                return (t',r:xs) )
                                     (ti,[])
                                     delta
                let result' = [(x,y,z) | (Just x,y,z) <- result]
                let optimal  = [(x,y,z) | (x,y,z) <- result', y == optimum]
                let best = maximum [x | (_,x,_) <- result']
                let optimal' = [(x,y,z) | (x,y,z) <- result', y == best]
                if (not . null) optimal
                then do
                    putStrLn $ "Optimal performance: " ++ show optimum
                    putStrLn $ show (length optimal) ++ " optimal deltas found."
                    appendFile "trace.txt" $ "OPTIMAL SOLUTIONS FOUND: \n"
                    appendFile "trace.txt" $ (unlines $ map (\(x,y,z) -> unlines $ (map showAxiom x) ++ ["  util:" ++ show y ++ ", comp len:" ++ show z ++ ", freevars: " ++ show (map freeVars x)]) $ optimal)
                    let delta = chooseBestDelta [x | x@(DArrow "Lang" _ _) <- ltm'] $ nub optimal
                    return $ Just delta
                else do
                    if len == fromIntegral sol && (not . null) optimal'
                    then do putStrLn $ "Best performance: " ++ show optimum
                            putStrLn $ show (length optimal) ++ " best deltas found."
                            let delta = chooseBestDelta [x | x@(DArrow "Lang" _ _) <- ltm'] $ nub optimal'
                            return $ Just delta
                    else return Nothing

        result2 <- occam $ delta12
        if result2 /= Nothing
        then return $ fromJust result2
        else if len == fromIntegral sol
             then do
                putStrLn $ "Trying with abstractions..."
                let tags = nub $ [t | (DArrow "Lang" _ y@(HsVar (UnQual (HsIdent t)))) <- ltm']
                --putStrLn $ "Tags: " ++ show tags
                let delta' = nub [[f] | f <- generateFuncsAbs tags posAxioms,
                                    isPure f,
                                    not (f `elem` ltm),
                                    not ((axLhs f) `elem` posrhs),
                                    length (freeVars f) <= freeVariables,
                                    singleDomain f,
                                    not (lhsIsVar f),
                                    --wmSize [] (axLhs f) >= wmSize [] (axRhs f),
                                    if tag=="Lang" then rhsIsTag f else True]
                putStrLn $ "Abstractions generated: " ++ show (length delta')
                result2 <- occam delta'
                if result2 /= Nothing
                  then return $ fromJust result2
                  else do
                    putStrLn $ "Trying with recursion"
                    let delta5 = 
                          [ [g1,g2]
                                | g1@(DArrow lang _ _) <- posAxioms,
                                  g2 <- map head $ group $ sort $ generateFuncsRec sol lang concepts tags g1,
                                  isPure g2,
                                  not (g2 `elem` ltm),
                                  g1 /= g2,
                                  singleDomain g2,
                                  length (freeVars g2) <= freeVariables
                            ]
                    putStrLn $ "Recursive deltas generated: " ++ show (length delta5)
                    -- writeFile "trace.txt" $ unlines $ map (unlines . map showAxiom) delta5
                    result5 <- occam $  delta5
                    if result5 /= Nothing
                    then return $ fromJust result5
                    else do
                        putStrLn $ "Maximum size reached: " ++ show sol
                        return posDelta
             else findDelta (len+1) agent neg pos

numberChange (DArrow _ (HsLit (HsInt _)) _) = True
numberChange (SArrow _ (HsLit (HsInt _)) _) = True
numberChange _ = False

-- for Language, the rhs must be a string
rhsIsTag (DArrow _ p (HsVar (UnQual _))) = True
rhsIsTag (DArrow _ p (HsLit (HsString _))) = True
rhsIsTag _ = False

lhsIsVar (DArrow _ (HsVar (Qual _ _)) _) = True
lhsIsVar _ = False

isPure (DArrow _ p q) = 
    let pvar = nub [HsVar e  |(HsVar e@(Qual _ _)) <- getSubExp p]
        qvar = nub [HsVar e  |(HsVar e@(Qual _ _)) <- getSubExp q]
    in null (qvar \\ pvar)
isPure (SArrow _ p q) = 
    let pvar = nub [HsVar e  |(HsVar e@(Qual _ _)) <- getSubExp p]
        qvar = nub [HsVar e  |(HsVar e@(Qual _ _)) <- getSubExp q]
    in null (qvar \\ pvar)
lhsIsSame (DArrow s (HsVar (Qual m _)) y) (DArrow t (HsVar (Qual n _)) q)
        = s == t && m == n
lhsIsSame (DArrow s (HsVar (Qual m _)) y) (SArrow t (HsVar (Qual n _)) q)
        = s == t && m == n
lhsIsSame (SArrow s (HsVar (Qual m _)) y) (SArrow t (HsVar (Qual n _)) q)
        = s == t && m == n
lhsIsSame (SArrow s (HsVar (Qual m _)) y) (DArrow t (HsVar (Qual n _)) q)
        = s == t && m == n
lhsIsSame (DArrow s x y) (DArrow t p q) = x == p && s == t
lhsIsSame (SArrow s x y) (SArrow t p q) = x == p && s == t
lhsIsSame _ _ = False
-----------------------

chooseBestDelta :: [Axiom] -> [([Axiom],Int,Int)] -> ([Axiom],Int,Int)
chooseBestDelta lx [] = ([],0,0)
chooseBestDelta lx [x] = x
chooseBestDelta lx deltas =
    let deltas' = [(ax,perf,len,sum (map countVars ax)) | (ax,perf,len)  <- deltas]
        maxVarCount = maximum [varCount | (ax,perf,len,varCount) <- deltas']
        deltas1 = [(ax,perf,len) | x@(ax,perf,len,vars) <- deltas', vars == maxVarCount]
    in
    if length deltas1 == 1
        then head deltas1
        else let deltas2 = [(ax,perf,len,length [p | p@(DArrow _ _ _) <- ax]) | (ax,perf,len)  <- deltas1]
                 maxArrowCount = maximum [arrows | (ax,perf,len,arrows) <- deltas2]
                 deltas3 = [(ax,perf,len) | x@(ax,perf,len,arrows) <- deltas2, arrows == maxArrowCount]
             in if length deltas3 == 1
                  then head deltas3
                  else let dx = concat [ax | (ax,_,_) <- deltas1]
                           ax = [(x,y) | (DArrow "Lang" x@(HsVar (UnQual _)) y@(HsVar (UnQual _))) <- lx ++ dx]
                           types = findTypeHierarchy ax
                           deltas4 = mostGeneral types deltas3
                       in  if length deltas4 == trace (show $ length deltas4) 1
                            then head deltas4
                            else
                              let minCompSize = minimum [x | (_,_,x) <- deltas4]
                                  deltas5 = [(x,y,z) | (x,y,z) <- deltas4, z == minCompSize]
                              in if length deltas5 == 1
                                    then head deltas5
                                    else if null deltas5
                                            then head $ sortBy (compare `on` (length . show)) deltas4
                                            else head $ sortBy (compare `on` (length . show)) deltas5

mostGeneral :: [HsExp] -> [([Axiom],Utility,Length)] -> [([Axiom],Utility,Length)]
mostGeneral [] ax = ax
mostGeneral (t:ts) ax = 
  let ax' = [(x,x1,x2,length ([y | y <- concatMap getSubExpAx x, y == t]
                              ++ [HsVar (UnQual (HsIdent v)) | (HsVar (Qual (Module v) _)) <- concatMap getSubExpAx x, HsVar (UnQual (HsIdent v)) == t]))
                            | (x,x1,x2) <- ax]
      tmax = maximum [x | (_,_,_,x) <- ax']
      result = [(x,i,comp) | (x,i,comp,x') <- ax', x' == tmax]
  in if length result == 1
          then result
          else mostGeneral ts result

findTypeHierarchy :: Eq a => [(a,a)] -> [a]
findTypeHierarchy [] = []
findTypeHierarchy ax = 
    let t = nub ax
        t' = nub $ concat [[x,y] | (x,y) <- t ]
        sortFunc x y = (case lookup x t of
                    Nothing -> GT
                    Just y' -> if y == y' then LT else sortFunc y' y)
        result = reverse $ sortBy sortFunc t'
    in  result

countVars :: Axiom -> Int
countVars (DArrow _ p q) = countVars' p + countVars' q
countVars (SArrow _ p q) = countVars' p + countVars' q
countVars' exp = length [x  | x@(HsVar (Qual _ _)) <- getSubExp exp]

performance :: Int -> Int -> Int 
            -> [Concept] -> [Axiom] -> [Axiom] -> TypeInfo
            -> [IP] -> [IP] -> [IP]
            -> Delta -> IO (TypeInfo,(Maybe Delta,Int,Int))
performance width depth sol concepts laxioms ltm ti negex pos ded func = do
    --putStrLn $ "TypeInfo size = " ++ show (sum $ map (length . snd) ti)
    --putStrLn $ showTypeInfo ti
    
    --putStrLn $ unlines $ map showAxiom func
    let ltm' = ltm ++ func
    (t1,ansPos) <- foldM (\(t,xs) (IP _ a b y) -> do
                            (t',result,len) <- findAnswerDecl concepts laxioms ltm' width depth t (a,b)
                            return (t',(result,y,len):xs)) (ti,[]) pos
    (t2,ansDed) <- foldM (\(t,xs) (IP _ a b y) -> do 
                            (t',result) <- findSolDecl concepts laxioms ltm' width depth t (a,b)
                            let len = length result
                            return (t',(result,y,len):xs)) (t1,[]) ded
    (t3,ansNeg) <- foldM (\(t,xs) (IP _ a b y) -> do
                            (t',result,len) <- findAnswerDecl concepts laxioms ltm' width depth t (a,b)
                            return (t',(result,y,len):xs)) (t2,[]) negex
    let posUtil = sum [y | (True,y,_) <- ansPos]
    let dedUtil = sum [y | (sol,y,_) <- ansDed,
                           (Right _,_,_) <- [head sol]]
    let negUtil = sum [y | (True,y,_) <- ansNeg]
    let util = posUtil + dedUtil - negUtil
    let compSize = sum ([x | (_,_,x) <- ansPos] ++ [x | (_,_,x) <- ansDed])
    if or [x | (x,_,_) <- ansNeg]
    then return (t3,(Nothing,util,compSize))
    else do
        if and [x | (x,_,_) <- ansPos]
        then do
            if null [x | (x@(Left _),_,_) <- (map head [x | (x,_,_) <- ansDed])]
            then return (t3,(Just func,util,compSize))
            else return (t3,(Nothing,util,compSize))
        else return (t3,(Nothing,util,compSize))

-- generate recursive axioms
-- one base case from IP
-- one recursive case of the form
--      f(big) ->> ... f(small) ...
--   where, f(small) has size 0, and size small < size big
generateFuncsRec :: Int -> Language -> [Concept] -> [String] -> Axiom -> [Axiom]
generateFuncsRec d lang concepts tags base = 
    let vconsts = "xyz"
        vars = [HsVar (Qual (Module m) (HsIdent [c])) | c <- vconsts, m <- tags]
        units = nub $ vars
                      ++ [exp | (l,arit,freq,exp) <- concepts, 
                                arit==0,
                                l==lang,
                                wmSize [] exp == 1]
        unary  = [exp | (l,arit,freq,exp) <- concepts, arit==1, l==lang]
        funcs = [f | (HsApp f _) <- getSubExp (axLhs base)]
        unary' = [f | f <- unary, not (f `elem` funcs)]
        binary = [exp | (l,arit,freq,exp) <- concepts, arit==2, l==lang]
        bigs = [ (i, generateExps i units unary' binary) | i <- [1..d] ]
        lhs = [(HsApp f x) | i <- [3..(d-3)],
                             x <- fromJust (lookup i bigs),
                             f <- funcs,
                             --containsVar x,
                             length [v | (HsVar (Qual _ v)) <- getSubExp x] == 1,
                             (HsApp f x) /= axLhs base]
        result = 
            [DArrow lang (HsApp f x) rhs
                | (HsApp f x) <- nub lhs,
                  let xsize = wmSize [] x,
                  let small = map (HsApp f)
                              $ filter (\y -> wmSize [] y < xsize)
                                [z | j <- [1..(xsize - 1)],
                                     z <- generateExps j (getSubExp x) unary' binary,
                                     containsVar z],
                  rhs <- concat [generateExpsRec i small units unary' binary | i <- [0..(d - xsize - 1)]],
                  rhs /= (HsApp f x),
                  containsVar rhs,
                  (not . null) [g | (HsApp g _) <- getSubExp rhs, g == f]
                  --rhs <- generateExps (d-i) (units ++ small) unary binary,
                  --or $ map (\x -> x `elem` (getSubExp rhs)) small
                    ]
    in result
        
-- generate abstractions of given axioms
-- e.g. for 1+(2+3), it returns x+(2+3), x+(y+z), etc.
generateFuncsAbs :: [String] -> [Axiom] -> Delta
generateFuncsAbs _ [] = []
generateFuncsAbs [] _ = []
generateFuncsAbs tags axioms = 
    let vconsts = "xyz"
        vars = [HsVar (Qual (Module m) (HsIdent [c])) | c <- vconsts, m <- tags]
        exps = [DArrow lang (foldl (\x' (v',s') -> replaceSubExp s' v' x') x v) (foldl (\x' (v',s') -> replaceSubExp s' v' x') y v) -- replace sub with v in ip
                  | (DArrow lang x y) <- axioms,
                    sub <- subsequences (nub (getSubExp x ++ getSubExp y)),
                    length sub > 0 && length sub < 4,
                    var <- subsequences vars,
                    length var == length sub,
                    -- not (sub `elem` vars),
                    let v = zip var sub
               ]
    in  exps
-- generate all axioms of given size
generateFuncsAll :: Language -> [Concept] -> Int -> [IP] -> Delta
generateFuncsAll _  _  len _   | len < 2 = []
generateFuncsAll lang concepts len pos =
    let variables = if lang == "Lang" then "" else "xyz"
        domains = [s | (_,_,f,(HsVar (UnQual (HsIdent s)))) <- concepts]
        parts = [lhs | (IP _ lhs _ _) <- pos] ++ [rhs | (IP _ _ rhs _) <- pos]
        units = nub $ [x | x <- (concatMap getSubExp parts), size x == 1]
                      ++ [HsVar (Qual (Module d) (HsIdent [c]))
                                | c <- variables, d <- domains]
                      ++ [exp | (l,arit,freq,exp) <- concepts, 
                                arit==0,
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
                        q <- fromJust (lookup i exps),
                        p <- fromJust (lookup (len - i) exps),
                        if i == (len-i) then p /= q else True
                  ]
    in  result -- ++ [x :-> y | (x :->> y) <- result]


generateExpsRec :: Int -> [HsExp] -> [HsExp] -> [HsExp] -> [HsExp] -> [HsExp]
generateExpsRec len _  _     _  _ | len < 0 = []
generateExpsRec 0   fs _     _  _  = fs
generateExpsRec 1   fs units f1 _  = [HsApp x f | x <- f1, f <- fs] ++ units
generateExpsRec 2   fs units _  f2
        = concat [[HsInfixApp f (HsQVarOp op) x,HsInfixApp x (HsQVarOp op) f]
                        | f <- fs,
                          (HsVar op) <- f2,
                          x <- units]
generateExpsRec 3   fs units f1 f2
    =   let exps2f = [HsApp x f | x <- f1, f <- fs]
            exps2  = units
            exps1  = generateExpsRec 2 fs units f1 f2
        in concat
            [[HsInfixApp x (HsQVarOp op) y, HsInfixApp y (HsQVarOp op) x]
                | x <- exps2f,
                  y <- exps2,
                  (HsVar op) <- f2]
            ++ [HsApp op x
                    | op <- f1,
                      x <- exps1]
generateExpsRec len fs units f1 f2 = undefined
{-
generateExpsRec len fs units f1 f2
    =   let exps2f = generateExpsRec (len-2) fs units f1 f2
            exps2 = generateExpsRec (len-2) [] units f1 f2
            exps1 = generateExpsRec (len-1) fs units f1 f2
        in concat
            [[HsInfixApp x (HsQVarOp op) y, HsInfixApp y (HsQVarOp op) x]
                | x <- exps2f,
                  y <- exps2,
                  (HsVar op) <- f2]
            ++ [HsApp op x
                    | op <- f1,
                      x <- exps1]
-}
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

containsVar :: HsExp -> Bool
containsVar e = (not . null)  [x | (HsVar (Qual _ x)) <- getSubExp e]

-- check if the axiom has only a single tag for each variable
-- returns false for (Dig.x + Num.x)
singleDomain :: Axiom -> Bool
singleDomain x = 
    let tags = [(tag,var) | (HsVar (Qual (Module tag) var)) <- getSubExp (axLhs x)]
        vars = nub $ map snd tags
    in all (==1)
        $ map (\v -> length $ nub [t | (t,v') <- tags, v'==v]) vars

-- get free variables, those that occur in lhs but not in rhs
freeVars :: Axiom -> [HsExp]
freeVars f =
    let rhsvars = [v | v@(HsVar (Qual _ _)) <- getSubExp (axRhs f)]
        lhsvars = [v | v@(HsVar (Qual _ _)) <- getSubExp (axLhs f)]
    in [v | v <- lhsvars, not (v `elem` rhsvars)]
