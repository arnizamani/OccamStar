-- Logic module

module Logic where

data MyBool =
      T
    | F
    | Var String
    | Neg MyBool
    | MyBool :| MyBool
    | MyBool :& MyBool
    | MyBool :> MyBool
    | MyBool :<> MyBool
        deriving (Eq,Show)

truth T = T
truth F = F
truth (Neg T) = F
truth (Neg F) = T
truth (Neg x) = truth (Neg (truth x))

truth (T :| x) = T
truth (x :| T) = T
truth (F :| x) = truth x
truth (x :| F) = truth x
truth (x :| y) = truth (truth x :| truth y)

truth (x :& F) = F
truth (F :& x) = F
truth (T :& x) = truth x
truth (x :& T) = truth x
truth (x :& y) = truth (truth x :& truth y)

truth (x :> T) = T
truth (F :> x) = T
truth (T :> x) = truth x
truth (x :> y) = truth (truth x :> truth y)

truth (T :<> T) = T
truth (F :<> F) = T
truth (T :<> F) = F
truth (F :<> T) = F
truth (x :<> y) = truth (truth x :<> truth y)

-------------------------------------------------------------------------------
--     VALIDITY
-------------------------------------------------------------------------------

valid (Var x) = Var x
-- Truth table
valid T = T
--valid F = F
--valid (Var x) = F

-- Negation truth table
--valid (Neg T) = F
valid (Neg F) = T
--valid (Neg (Neg F)) = F
valid (Neg (Neg T)) = T
--valid (Neg (Var x)) = F
--valid (Neg (Neg (Var x))) = F

-- Negation
valid (Neg (Neg x)) = valid x
valid (Neg (x :| y)) = valid (Neg x :& Neg y)
valid (Neg (x :& y)) = valid (Neg x :| Neg y)
valid (Neg (x :> y)) = valid (x :& Neg y)
valid (Neg (Neg x :& Neg y)) = valid (x :| y)
valid (Neg (x :& Neg y)) = valid (Neg x :| y)
valid (Neg (Neg x :& y)) = valid (x :| Neg y)
valid (Neg (x :| y)) = valid (Neg x :& Neg y)
valid (Neg (Neg x :| Neg y)) = valid (x :& y)
valid (Neg (x :| Neg y)) = valid (Neg x :& y)
valid (Neg (Neg x :| y)) = valid (x :& Neg y)
valid (Neg (x :& y)) = valid (Neg x :| Neg y)
valid (Neg (x :<> T)) = valid (Neg x)
valid (Neg (T :<> x)) = valid (Neg x)
valid (Neg (F :<> x)) = valid x
valid (Neg (x :<> F)) = valid x
valid (Neg (x :<> y)) = valid (Neg (y :<> x))
valid (Neg (Neg x :<> x)) = T
valid (Neg (x :<> Neg x)) = T

-- OR truth table
--valid (F :| F) = F
valid (x :| T) = T
valid (T :| x) = T
valid (x :| Neg x) = T
valid (Neg x :| x) = T
valid (x :| (Neg x :| y)) = T
valid (x :| (y :| Neg x)) = T
valid ((x :| Neg x) :| y) = T
valid ((x :| y) :| Neg x) = T
valid (y :| (x :| Neg x)) = T
valid (y :| (Neg x :| x)) = T
valid ((y :| x) :| Neg x) = T
valid ((y :| Neg x) :| x) = T
valid ((Neg x :| x) :| y) = T
valid ((Neg x :| y) :| x) = T
valid (Neg x :| (y :| x)) = T
valid (Neg x :| (x :| y)) = T

-- OR
valid (F :| x) = valid x
valid (x :| F) = valid x
valid (x :| x) = valid x
valid (x :| y) = valid (y :| x)
valid ((x :| y) :| z) = valid (x :| (y :| z))
valid (x :| (y :| z)) = valid ((x :| y) :| z)
valid ((x :& y) :| (x :& z)) = valid (x :& (y :| z))
valid (x        :| (y :& z)) = valid ((x :| y) :& (x :| z))
valid (x     :| y)     = valid (Neg (Neg x :& Neg y))
valid (Neg x :| y)     = valid (Neg (x :& Neg y))
valid (x     :| Neg y) = valid (Neg (Neg x :& y))
valid (Neg (Neg x :& Neg y))  = valid (x :| y)
valid (Neg (x :& Neg y)) = valid (Neg x :| y)
valid (Neg (Neg x :& y)) = valid (x :| Neg y)
valid (Neg x :| y) = valid (x :> y)
valid (Neg x :| Neg y) = valid (Neg (x :& y))
valid (x :| y) = valid x
valid (x :| y) = valid y
valid (x :| y) = valid (valid x :| valid y)

-- AND truth table
valid (T :& T) = T
--valid (x :& F) = F
--valid (F :& x) = F
--valid (x :& Neg x) = F
--valid (Neg x :& x) = F

-- AND
valid (T :& x) = valid x
valid (x :& T) = valid x
valid (x :& y) = valid (valid x :& valid y)
valid (x :& y) = valid (y :& x)
valid ((x :& y) :& z) = valid (x :& (y :& z))
valid (x :& (y :& z)) = valid ((x :& y) :& z)
valid ((x :| y) :& (x :| z)) = valid (x :| (y :& z))
valid (x        :& (y :| z)) = valid ((x :& y) :| (x :& z))
valid (Neg x :& Neg y) = valid (Neg (x :| y))
valid (x :& Neg y) = valid (Neg (x :> y))
valid (x :& y) = valid (Neg (Neg x :| Neg y))
valid (Neg x :& y) = valid (Neg (x :| Neg y))
valid (x :& Neg y) = valid (Neg (Neg x :| y))

-- IMP truth table
--valid (T :> F) = F
valid (x :> T) = T
valid (F :> x) = T
valid (x :> x) = T
valid ((T :> x) :<> x) = T
valid ((x :> F) :<> (Neg x)) = T
valid (x :> (x :| y)) = T
valid (y :> (x :| y)) = T

-- IMP
valid (T :> x) = valid x
valid ((x :> F)) = valid (Neg x)
valid (Neg y :> Neg x) = valid (x :> y)
valid (Neg x :> x) = valid x
valid (x :> Neg x) = valid (Neg x)
valid ((x :& y) :> x) = T
valid ((x :& y) :> y) = T
valid ((x :<> y) :> (x :> y)) = T
valid ((x :<> y) :> (y :> x)) = T
valid (x :> (y :> x)) = T
valid (x :> y) = valid y
valid (x :> y) = valid (Neg x)
valid (Neg (Neg x) :> y) = valid (x :> y)
valid (x :> (Neg (Neg y))) = valid (x :> y)

valid (x :> y) = valid (Neg x :| y)
valid (x :> y) = valid (Neg x)
valid (x :> y) = y

-- EQV truth table
valid (T :<> T) = T
valid (F :<> F) = T
--valid (T :<> F) = F
--valid (F :<> T) = F
valid (x :<> x) = T
valid ((x :| x) :<> x) = T
valid ((x :& x) :<> x) = T
valid (x :<> x) = T
--valid (Neg x :<> x) = F
--valid (x :<> Neg x) = F
valid (Neg x :<> Neg x) = T
valid ((Neg (Neg x)) :<> x) = T
valid (Neg (Neg (Neg x)) :<> Neg x) = T

-- EQV
valid (x :<> T) = valid x
valid (T :<> x) = valid x
valid (F :<> x) = valid (Neg x)
valid (x :<> F) = valid (Neg x)
valid (x :<> y) = valid (y :<> x)
valid (x :<> y) = valid (valid x :<> y)
valid (x :<> y) = valid (x :<> valid y)
valid (x :<> y) = valid (valid x :<> valid y)
valid ((x :<> y) :<> z) = valid (x :<> (y :<> z))
valid (x :<> (y :<> z)) = valid ((x <> y) :<> z)
valid ((Neg x :| y) :<> (x :> y)) = T
valid ((x :& Neg y) :<> Neg (x :> y)) = T
valid ((Neg y :> Neg x) :<> (x :> y)) = T
valid ((x :> Neg x) :<> Neg x) = T
valid ((Neg x :> x) :<> x) = T



valid x = x



rev [] = []
rev (x:xs) = (rev xs) P.++ [x]

-- [] ++ [] = []
-- x ++ [] = []
-- [] ++ x = x
-- [x] ++ [y] = [x,y]
-- [x] ++ y = x:y
-- (x:xs) ++ ys =  x : (xs ++ ys)
-- (x ++ y) ++ z = x ++ (y ++ z)
-- x ++ (y ++ z) = (x ++ y) ++ z

