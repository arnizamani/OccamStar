module Test where

import Data.List

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

