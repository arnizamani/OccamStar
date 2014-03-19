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

type Utility = Int
type Delta = [Axiom]
noSrc = SrcLoc "" 0 0

-- | Free variables are those that occur in lhs but not in rhs.
-- | For example, the axiom (x*0=0) has one free variable.
freeVariables = 1

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
        let (Just agent@(Agent c (width,depth,sol) (axioms,concepts))) = agent'
        (pos,neg) <- parseTrainingFile ipfile
        let examples = pos ++ neg
        let units = makeUnitConcepts examples
        let unary = makeUnaryConcepts examples
        let binary = makeBinaryConcepts examples
        let newc = units ++ unary ++ binary
        let t = foldl (flip insertInConcepts) [] (concepts ++ newc)
        let newAgent = (Agent c (width,depth,sol) (axioms,t))
        -- putStrLn $ show $ wmSize concepts $ lhs $ head pos
        (delt,util,len) <- findDelta 0 newAgent neg pos
        if null delt
        then do
            --putStrLn $ "All examples solved. Nothing to learn."
            saveAgent newAgent agentfile
        else do
            putStrLn $ "\nLearned this rule: "
            putStrLn . unlines . map showAxiom $ delt
            putStrLn . show $ util
            putStrLn $ "Do you want the agent to remember it? (Y/n) "
            c <- getChar
            if c == 'n' || c == 'N'
            then  saveAgent newAgent agentfile
            else do
                let (Agent c (width,depth,sol) (axioms,concepts)) = newAgent
                let newltm = union axioms delt
                let newAgent' = Agent c (width,depth,sol) (newltm,concepts)
                saveAgent newAgent' agentfile
                putStrLn $ "Stored in memory."
     _  -> printMessage
    
        
findDelta :: Int -> Agent -> [IP] -> [IP] -> IO ([Axiom],Utility,Length)
findDelta len ag _ _ | len < 0 = return ([],0,0)
findDelta len agent neg posex = do
    let (Agent c (width,depth,sol) (ltm',concepts)) = agent
    let langs = [lang |  (IP lang _ _ _) <- posex]
    let pos = [x  |  x@(IP _ p e v) <- posex,
                     e /= HsVar (UnQual (HsIdent "x"))]
    let ded = [x  |  x@(IP _ p (HsVar (UnQual (HsIdent "x"))) v) <- posex]
    let posAxioms = [DArrow s p e | IP s p e v <- pos]
    let optimum   = sum ([v | IP _ p e v <- pos, v > 0]
                          ++ [v | IP _ p e v <- ded, v > 0])
    if optimum < 1
    then return ([],0,0)
    else do
    if len > sum (map size pos)
    then do putStrLn $ "Size exceeded from " ++ show (sum (map size pos))
            return (posAxioms,sum [v | (IP _ _ _ v) <- pos, v > 0],0)
    else do
    if len > fromIntegral sol
    then do putStrLn $ "Maximum size reached: " ++ show sol
            return ([],0,0)
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
            return ([],0,0)
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
                return ([],0,0)
            else findDelta 1 agent neg pos
    else do
        putStrLn $ "Searching at length: " ++ show len
        let posrhs = [e | IP s p e v <- pos, not (containsVar e)]
        let funcs' = [ (i, [f | f <- generateFuncsAll lang concepts i pos,
                               isPure f,
                               axSize concepts f == i,
                               not (f `elem` ltm),
                               not ((axLhs f) `elem` posrhs),
                               --freeVars f <= freeVariables,
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
        
        --let delta1 = nubBy (\(x:[]) (y:[]) -> lhsIsSame x y && axRhs x == axRhs y) delta1''
        putStrLn $ "    generated functions: " ++ show (length delta1 + length delta2)
        --appendFile "temp.txt"
        --  $ unlines $ map (\x -> concat . intersperse ", " $ map showAxiom x) delta
        let ti = [] :: TypeInfo
        let func delta = do
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
                    appendFile "trace.txt" $ (unlines $ map (\(x,y,z) -> unlines $ (map showAxiom x) ++ [" " ++ show y ++ ", " ++ show z]) $ optimal)
                    let delta = chooseBestDelta [x | x@(DArrow "Lang" _ _) <- ltm'] $ nub optimal
                    return $ Just delta
                else do
                    if len == fromIntegral sol && (not . null) optimal'
                    then do putStrLn $ "Best performance: " ++ show optimum
                            putStrLn $ show (length optimal) ++ " best deltas found."
                            let delta = chooseBestDelta [x | x@(DArrow "Lang" _ _) <- ltm'] $ nub optimal'
                            return $ Just delta
                    else return Nothing

        result2 <- func $ delta1 ++ delta2
        if result2 /= Nothing
        then return $ fromJust result2
        else do
            --result1 <- func delta1
            --if result1 /= Nothing
            --then return $ fromJust result1
            --else
                findDelta (len+1) agent neg pos

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
    putStrLn $ unlines $ map showAxiom func
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

generateFuncsAll :: Language -> [Concept] -> Int -> [IP] -> Delta
generateFuncsAll _  _  len _   | len < 2 = []
generateFuncsAll lang concepts len pos =
    let variables = if lang == "Lang" then "" else "xyz"
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

containsVar :: HsExp -> Bool
containsVar e = not $ null [x | (HsVar (Qual _ x)) <- getSubExp e]

-- check if the axiom has only a single tag for each variable
-- returns false for (Dig.x + Num.x)
singleDomain :: Axiom -> Bool
singleDomain x = 
    let tags = [(tag,var) | (HsVar (Qual (Module tag) var)) <- getSubExp (axLhs x)]
        vars = nub $ map snd tags
    in all (==1)
        $ map (\v -> length $ nub [t | (t,v') <- tags, v'==v]) vars

-- count the number of free variables
freeVars :: Axiom -> Int
freeVars f =
    let rhsvars = [v | v@(HsVar (Qual _ _)) <- getSubExp (axRhs f)]
        lhsvars = [v | v@(HsVar (Qual _ _)) <- getSubExp (axLhs f)]
    in length [v | v <- lhsvars, not (v `elem` rhsvars)]
