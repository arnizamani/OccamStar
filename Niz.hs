-- author: Abdul Rahim Nizamani (c) 2013
-- ITIT, GU, Gothenburg, Sweden

-- Common helper functions not already available in standard Haskell

module Niz where
import Prelude hiding (catch)
import System.IO
import Data.List
import Text.ParserCombinators.ReadP
import System.Directory
import Control.Exception (catch, IOException)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


class Size a where
    size :: a -> Int

instance Size [a] where
    size xs = length xs

instance Size (Set a) where
    size x = Set.size x

instance Size (Map a b) where
    size x = Map.size x

notnull = not . null

-------------------------------------------------------------------------------
--           List functions
-------------------------------------------------------------------------------

ngrams :: [a] -> Int -> [[a]]
ngrams [] _ = []
ngrams xs i | length xs < i = []
ngrams (x:xs) i = (x:(take (i-1) xs)):ngrams xs i

bigrams :: [a] -> [(a,a)]
bigrams (_:[]) = []
bigrams x = zip x (tail x)

-- filter and then map in one run, reduces complexity from 2n to n
filtermap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtermap f m xs = [m x | x <- xs, f x]

-- map and then filter in one run, reduces complexity from 2n to n
mapfilter :: (a -> b) -> (b -> Bool) -> [a] -> [b]
mapfilter _ _ [] = []
mapfilter m f (x:xs) = f result ? (result:(mapfilter m f xs)) $ (mapfilter m f xs)
                 where result = m x

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

allPairs x = powerzip x x

uniquePairs a = [(x,y) | x <- a, y <- a, x/=y, x<y]

powerzip :: [a] -> [b] -> [(a,b)]
powerzip [] _ = []
powerzip _ [] = []
powerzip (x:xs) ys = zip (repeat x) ys ++ powerzip xs ys

-- | [1,2,3] -> [ ([],1,[2,3]), ([1],2,[3]), ([1,2],3,[]) ]
breaklist :: [a] -> [([a],a,[a])]
breaklist xs = break' [] xs
  where break' _ [] = []
        break' ws (x:xs) = (ws,x,xs) : break' (ws ++ [x]) xs

-- Function to simulate if-then-else
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

zipIf :: [Bool] -> [a] -> [a] -> [a]
zipIf = zipWith3 if'

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

-- | Divide a list into elements that satisfy the condition and those that do not.
-- This is different from break, as all elements are checked.
-- Original order may not be preserved.
divide :: (a -> Bool) -> [a] -> ([a],[a])
divide f [] = ([],[])
divide f (x:xs) = f x ? (x:a,b) $ (a, x:b)
    where (a,b) = divide f xs

-- | get unique values, i.e., remove all duplicate values
unique :: (Ord a) => [a] -> [a]
unique = uniqueBy (compare)

uniqueBy :: (a -> a -> Ordering) -> [a] -> [a]
uniqueBy eq x = concat . filter (\a -> length a == 1) . groupBy (\a b -> eq a b == EQ) . sortBy eq $ x

-- General parsing functions
readWithoutBrackets :: ReadP b -> ReadP b
readWithoutBrackets p = do
    r <- p
    return r

readWithBrackets :: ReadP b -> ReadP b
readWithBrackets p = do
    char '('
    r <- readWithBrackets p <++ p
    char ')'
    return r
wschars = " \t\r\n"
strip :: String -> String
strip = lstrip . rstrip
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x wschars
                            then lstrip xs
                            else s
rstrip = reverse . lstrip . reverse

readFileSafe :: FilePath ->  TextEncoding -> IO String
readFileSafe f enc = readFileSafe' f enc
     `catch`
        (\e -> do putStrLn ("Error in reading file " ++ f)
                  putStrLn $ "Exception: " ++ show (e :: IOException)
                  return "")

-- | Read file after checking its existence and permissions
readFileSafe' :: FilePath ->  TextEncoding -> IO String
readFileSafe' f enc = do
       e <- doesFileExist f
       if (not e)
         then do
           putStrLn $ "File " ++ f ++ " does not exist."
           return ""
         else do
           p <- getPermissions f
           if (not (readable p))
             then do
               putStrLn $ "Unable to read file " ++ f ++ "."
               return ""
             else do
               h <- openFile f ReadMode
               hSetEncoding h enc
               text <- hGetContents h
               putStrLn $ "Reading file " ++ show f ++ ", file length= " ++ show (length text)
               hClose h
               return text

