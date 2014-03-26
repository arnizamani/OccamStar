-------------------------------------------------------------------------------
--   PARSING FUNCTIONS
-------------------------------------------------------------------------------

module Parsing where
import Instances
import Haskell
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Data.List
import Data.Word
import Data.Char
import Data.Maybe
import System.IO
import Control.Monad (liftM)
import Niz


showConcept :: Concept -> String
showConcept (lang,arity,freq,exp) = 
    "(" ++ lang ++ "," ++ show arity ++ "," ++ show freq ++ "," ++ showExp exp ++ ")"

-- get all unit concepts from the set of examples
makeUnitConcepts :: [IP] -> [Concept]
makeUnitConcepts ips = nub $ concatMap concepts' ips
  where
    concepts' (IP lang lhs rhs _) = 
        [(lang,0,1,exp) | exp <- getSubExp lhs]
          ++ [(lang,0,1,exp) | exp <- getSubExp rhs]
-- get all unary concepts (unary functions) from the set of examples
makeUnaryConcepts :: [IP] -> [Concept]
makeUnaryConcepts ips = nub $ concatMap concepts' ips
  where
    concepts' (IP lang lhs rhs _) = 
        [(lang,1,1,f) | (HsApp f@(HsVar _) _) <- getSubExp lhs]
        ++ [(lang,1,1,f) | (HsApp f@(HsCon _) _) <- getSubExp lhs]
        ++ [(lang,1,1,f) | (HsApp f@(HsVar _) _) <- getSubExp rhs]
        ++ [(lang,1,1,f) | (HsApp f@(HsCon _) _) <- getSubExp rhs]

-- get all binary concepts (infix functions) from the set of examples
makeBinaryConcepts :: [IP] -> [Concept]
makeBinaryConcepts ips = nub $ concatMap concepts' ips
  where
    concepts' (IP lang lhs rhs _) = 
        [(lang,2,1,HsVar f) | (HsInfixApp _ (HsQVarOp f) _) <- getSubExp lhs]
        ++ [(lang,2,1,HsVar f) | (HsInfixApp _ (HsQConOp f) _) <- getSubExp lhs]
        ++ [(lang,2,1,HsVar f) | (HsInfixApp _ (HsQVarOp f) _) <- getSubExp rhs]
        ++ [(lang,2,1,HsVar f) | (HsInfixApp _ (HsQConOp f) _) <- getSubExp rhs]

insertInConcepts :: Concept -> [Concept] -> [Concept]
insertInConcepts c [] = [c]
insertInConcepts c@(l,a,f,e) cs = 
    let nf = f + sum [f' | (l',a',f',e') <- cs, l' == l, a' == a, e' == e]
        c' = (l,a,nf,e)
        result = c' : [x | x@(l',a',f',e') <- cs, (l' /= l || a' /= a || e' /= e)]
    in result

-- | Parse the IP file, return positive and negative examples
parseConceptsFile :: FilePath -> IO [Concept]
parseConceptsFile file = do
    text <- readFileSafe file utf8
    let inputs =
              concatMap parseCInput
            . map (\(x:xs) -> init xs)
            . filter (\(x:xs) -> x == '(' && (take 1 $ reverse xs) == ")")
            . filter (not . null)
            . map strip
            $ lines text
    c' <- mapM parseConcept $ inputs
    let c = concat c'
    putStrLn $ "Concepts: " ++ show (length c)
    return c

-- | Parse the IP file, return positive and negative examples
parseTrainingFile :: FilePath -> IO ([IP],[IP])
parseTrainingFile file = do
    text <- readFileSafe file utf8
    let inputs =
              concatMap parseInput
            . map (\(x:xs) -> init xs)
            . filter (\(x:xs) -> x == '(' && (take 1 $ reverse xs) == ")")
            . filter (not . null)
            . map strip
            $ lines text
    putStrLn $ show (length inputs) ++ " axioms read from IP file."
    p <- mapM parseIP $ inputs
    let tags = nub $ map getTag $ concat p
    if length tags > 1
    then do
        putStrLn $ "Error in IP: multiple tags used."
        return ([],[])
    else do
    let p' = concat p
    let pos = filter (\x -> val x >= 0) p'
    let neg = filter (\x -> val x < 0) p'
    putStrLn $ "pos: "
    putStrLn $ concat $ intersperse "," $ map ("    " ++)
                      $ map prettyIP pos
    putStrLn $ "neg: "
    putStrLn $ concat $ intersperse "," $ map ("    " ++)
                      $ map prettyIP neg
    return $ (pos,neg)

parseConcept :: Concept' -> IO [Concept]
parseConcept (lang,0,freq,str) = do
    let a' = parseModule $ "main = " ++ str
    if not (parseSuccess a')
    then return []
    else do
    let a1' = (\(HsModule _ _ _ _ dec) -> dec) $ getModule a'
    let exps = [lhs | (HsPatBind _ _ (HsUnGuardedRhs lhs) _) <- a1']
    if null exps
    then return []
    else do
    let exp = head exps
    return [(lang,0,freq,normalExp False exp)]
parseConcept (lang,arit,freq,str@(s:_)) | isLetter s
        = return [(lang,arit,freq,HsVar (UnQual (HsIdent str)))]
parseConcept (lang,arit,freq,str)
        = return [(lang,arit,freq,HsVar (UnQual (HsSymbol str)))]

parseIP :: IP' -> IO [IP]
parseIP (name,a,b,c) = do
    let a' = parseModule $ "main = " ++ a
    let b' = parseModule $ "main = " ++ b
    if not (parseSuccess a') || not (parseSuccess b')
    then return []
    else do
    let a1' = (\(HsModule _ _ _ _ dec) -> dec) $ getModule a'
    let b1' = (\(HsModule _ _ _ _ dec) -> dec) $ getModule b'

    if null a1' || null b1'
    then return []
    else do

    let a1 = head a1'
    let b1 = head b1'
    --putStrLn $ show a1
    --putStrLn $ show b1
    let (p,e) = mergeHsDecl a1 b1
    let result = [IP name (normalExp True p) (normalExp True e) c]
    return result

-- parses a line from Agent file, 3*0 ->>_Arith 0
parseEq :: String -> IO [Axiom]
parseEp "" = return []
parseEq s = do
    let index1 = findInfixIndex " ->>_" s
    
    if index1 == Nothing
    then do
        let index2 = findInfixIndex " ->_" s
        if index2 == Nothing
        then return []
        else do
            let a = take (fromJust index2) s
            let rest = drop (fromJust index2 + 4) s
            let s = takeWhile (/= ' ') rest
            let b = dropWhile (== ' ') $ dropWhile (/= ' ') rest
            if null a || null b || null s
            then return []
            else do
                let a' = parseModule $ "main = " ++ a
                let b' = parseModule $ "main = " ++ b
                if not (parseSuccess a') || not (parseSuccess b')
                then return []
                else do
                let a1 = (\(HsModule _ _ _ _ dec) -> dec) $ getModule a'
                let b1 = (\(HsModule _ _ _ _ dec) -> dec) $ getModule b'
                let a1' = [e | (HsPatBind _ _ (HsUnGuardedRhs e) _) <- a1]
                let b1' = [e | (HsPatBind _ _ (HsUnGuardedRhs e) _) <- b1]
                let result = [SArrow s (normalExp False (head a1')) (normalExp False (head b1'))]
                --appendFile "temp.txt" (show result ++ "\n")
                return result
    else do
    let a = take (fromJust index1) s
    let rest = drop (fromJust index1 + 5) s
    let s = takeWhile (/= ' ') rest
    let b = dropWhile (== ' ') $ dropWhile (/= ' ') rest
    if null a || null b || null s
    then do
        return []
    else do
    let a' = parseModule $ "main = " ++ a
    let b' = parseModule $ "main = " ++ b
    if not (parseSuccess a') || not (parseSuccess b')
    then return []
    else do
    let a1 = (\(HsModule _ _ _ _ dec) -> dec) $ getModule a'
    let b1 = (\(HsModule _ _ _ _ dec) -> dec) $ getModule b'

    let a1' = [e | (HsPatBind _ _ (HsUnGuardedRhs e) _) <- a1]
    let b1' = [e | (HsPatBind _ _ (HsUnGuardedRhs e) _) <- b1]
    let result = [DArrow s (normalExp False (head a1')) (normalExp False (head b1'))]
    --appendFile "temp.txt" (show result ++ "\n")
    return result
    {-
    let a1' = [ e | e@(HsPatBind _ _ (HsUnGuardedRhs (HsApp (HsVar (UnQual n)) p)) _) <- a1]

    let result1 = [HsMatch (SrcLoc "" 0 0) n [expToPat p] (HsUnGuardedRhs rhs) []
                    |   (HsPatBind _ _ (HsUnGuardedRhs (HsApp (HsVar (UnQual n)) p)) _) <- a1',
                        (HsPatBind _ _ (HsUnGuardedRhs rhs) _) <- b1]
    let result2 = [HsPatBind (SrcLoc "" 0 0) (expToPat lhs) (HsUnGuardedRhs rhs) []
                    |   (HsPatBind _ _ (HsUnGuardedRhs lhs) _) <- a1 \\ a1',
                        (HsPatBind _ _ (HsUnGuardedRhs rhs) _) <- b1]
    
    return $ [HsFunBind [normalMatch x] | x <- result1]
              ++ result2
    -}

isHsPatBind (HsPatBind _ _ (HsUnGuardedRhs _) _) = True
isHsPatBind _ = False

parseCInput :: String -> [Concept']
parseCInput "" = []
parseCInput s | not (',' `elem` s) = []
parseCInput s = if not (null name) && (>=0) arity && (>=0) freq && not (null value)
                  then [(name,arity,freq,value)]
                  else []
    where
        name = strip $ takeWhile (/= ',') s
        s1 = strip $ drop 1 $ dropWhile (/= ',') s
        arity' = strip $ takeWhile (/=',') s1
        arity = if not (null arity') && all isDigit arity'
                    then (read arity' :: Int)
                    else -1
        s2 = strip $ drop 1 $ dropWhile (/= ',') s1
        freq' = strip $ takeWhile (/=',') s2
        freq = if not (null freq') && all isDigit freq'
                    then (read freq' :: Int)
                    else -1
        value = strip $ drop 1 $ dropWhile (/= ',') s2

parseInput :: String -> [IP']
parseInput "" = []
parseInput s | not (',' `elem` s) = []
parseInput s' = if not (null name) && not (null term1) && not (null term2) -- && (/=0) value
                then [(name,term1, term2, value)]
                else []
    where
        name = strip $ takeWhile (/= ',') s'
        s = strip $ drop 1 $ dropWhile (/= ',') s'
        (a',b') = break (==',') $ reverse s
        b = reverse (if not (null b') then tail b' else b')
        a = reverse a'
        value = if null a
                    then 0
                    else if head a == '-'
                            then if all isDigit (tail a) then (read a :: Int) else 0
                            else if all isDigit a then (read a :: Int) else 0
        (x',y') = if (take 1 $ reverse b) == "]"
                    then let (p,q) = break (=='[') $ reverse b
                         in if null q
                                then ([],[])
                                else (p ++ [head q], tail q)
                    else break (==',') $ reverse b
        term1 = reverse $ if not (null y') then tail y' else y'
        term2 = reverse x'

-- | Read and parse an agent from a file
parseAgent :: FilePath -> IO (Maybe Agent)
parseAgent f = do
        text <- readFileSafe f utf8
        let com = takeWhile (\x -> not (isPrefixOf "-}" (strip x)))
                    $ filter (not . null . strip) $ lines text
        let rest = dropWhile (\x -> not (isPrefixOf "-}" x))
                    $ filter (not . null) $ map strip $ lines text
        if null rest
        then do
            putStrLn $ "Module not found. Failed to parse file " ++ f ++ "."
            return Nothing
        else do

        let (modLine:restLines) = rest
        if null restLines
        then do
            putStrLn $ "Error: Empty agent file " ++ f ++ "."
            putStrLn $ "       Agent file must define width, depth, solution, filename."
            return Nothing
        else do
        let axiomLines =
              filter  (\x ->     findInfixIndex " ->>_" x /= Nothing
                              || findInfixIndex " ->_"  x /= Nothing)
                      restLines
        let conceptLines =
              filter  (\x ->     findInfixIndex " ->>_" x == Nothing
                              && findInfixIndex " ->_"  x == Nothing)
                      restLines
        axioms' <- mapM parseEq axiomLines
        let axioms = nub $ concat axioms'
        let width = getWidth axioms
        let depth = getDepth axioms
        let sol   = getSolution axioms
        let inputs =
                concatMap parseCInput
              . map (\(x:xs) -> init xs)
              . filter (\(x:xs) -> x == '(' && (take 1 $ reverse xs) == ")")
              . filter (not . null)
              . map strip
              $ conceptLines
        concepts <- (liftM concat) $ mapM parseConcept $ inputs
        --concepts  <- parseConceptsFile cfile
        if width == 0 || depth == 0
        then do
            putStrLn $ "Error: Width and depth parameters must be greater than zero."
            return Nothing
        else do
        putStrLn $ "Parsed agent "
        putStrLn $ "Width = " ++ show width
        putStrLn $ "Depth = " ++ show depth
        return $ Just $ Agent (unlines com) (width, depth, sol) (axioms,concepts)
        --return Nothing


parseSuccess :: ParseResult HsModule -> Bool
parseSuccess (ParseOk m) = True
parseSuccess _           = False

getWidth :: [Axiom] -> Int
getWidth axioms = let r = [x | (DArrow "Param" (HsVar (UnQual (HsIdent "Width"))) (HsLit (HsInt x))) <- axioms]
                  in if null r then 0 else (fromIntegral $ head r)

getDepth :: [Axiom] -> Int
getDepth axioms = let r = [x | (DArrow "Param" (HsVar (UnQual (HsIdent "Depth"))) (HsLit (HsInt x))) <- axioms]
                 in if null r then 0 else (fromIntegral $ head r)

getSolution :: [Axiom] -> Int
getSolution axioms = let r = [x | (DArrow "Param" (HsVar (UnQual (HsIdent "Solution"))) (HsLit (HsInt x))) <- axioms]
                  in if null r then 0 else (fromIntegral $ head r)

-- merge the two HsDecl, one as lhs and one as rhs
mergeHsDecl :: HsDecl -> HsDecl -> (HsExp,HsExp)
mergeHsDecl (HsPatBind _ _ (HsUnGuardedRhs (HsApp (HsVar (UnQual name)) pat)) _) (HsPatBind _ _ (HsUnGuardedRhs exp) _)
    -- = normalDec $ HsFunBind [HsMatch (SrcLoc "" 0 0) name [expToPat pat] (HsUnGuardedRhs exp) []]
    = ((HsApp (HsVar (UnQual name)) pat), exp)
mergeHsDecl (HsPatBind _ _ (HsUnGuardedRhs lhs) _) (HsPatBind _ _ (HsUnGuardedRhs exp) _)
    -- = normalDec $ HsPatBind (SrcLoc "" 0 0) (expToPat lhs) (HsUnGuardedRhs exp) []
    = (lhs, exp)

getModule :: ParseResult HsModule -> HsModule
getModule (ParseOk m) = m

findInfixIndex :: (Eq a) => [a] -> [a] -> Maybe Int
findInfixIndex needle haystack
    = (\x -> if null x then Nothing else Just (fst $ head x))
      . dropWhile (\(_,x) -> not $ isPrefixOf needle x) 
        $ zip [0..] (tails haystack)

prettyIP :: IP -> String
prettyIP (IP name p e v) = name ++ ": " ++ showExp p ++ " = " ++ showExp e ++ ", " ++ show v

