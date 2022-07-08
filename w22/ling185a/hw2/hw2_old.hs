module Assignment02 where

-- Imports everything from the Recursion module. 
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), (<=), (>=), (++), not, Bool(..), Char, Int)

-- Notice how Haskell's bracketing conventions let us:
--      (a) write 'Numb -> Numb -> Numb' as a shorthand for 'Numb -> (Numb -> Numb)', and
--      (b) write 'bigger x y' as a shorthand for '(bigger x) y'.
bigger :: Numb -> Numb -> Numb
bigger = \n -> \m -> case n of
                     Z -> m
                     S n' -> case m of {Z -> n; S m' -> S (bigger n' m')}

numbToInt :: Numb -> Int
numbToInt = \n -> case n of
                  Z -> 0
                  S n' -> 1 + numbToInt n'

intToNumb :: Int -> Numb
intToNumb = \x -> case (x <= 0) of {True -> Z; False -> S (intToNumb (x-1))}

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

-- notes to self: syntax is mult m n
-- case Z handles m * 0
-- handle multiplication by doing m + (n - 1)m

mult :: Numb -> Numb -> Numb
mult = \n -> (\m -> case n of
                    Z -> Z
                    S n' -> add m (mult m n')
             )

sumUpTo :: Numb -> Numb
sumUpTo = \n -> (case n of
                 Z -> Z
                 S n' -> add n (sumUpTo n')
                )

-- equality isn't necessarily a recursive operation like mult
-- or sumUpTo; hopefully I don't lose points for this method
-- of implementation

-- ((2 * bigger m n) - m - n) is > 0 if m != n
-- hence <= 0 -> true, > 0 -> false

equal :: Numb -> Numb -> Bool
equal = \n -> (\m -> (2 * numbToInt (bigger m n) - (numbToInt m) - (numbToInt n)) <= 0)

count :: (a -> Bool) -> [a] -> Numb
count = \m -> (\n -> case n of
                     [] -> Z
                     x : rest -> case (m x) of {True -> add one (count m rest);
                                                False -> count m rest}
              )

listOf :: Numb -> a -> [a]
listOf = \n -> (\m -> case n of
                      Z -> []
                      S n' -> (m : listOf n' m)
               )
               
addToEnd :: a -> [a] -> [a]
addToEnd = \m -> (\n -> case n of
                        [] -> (m : [])
                        x : rest -> (x : addToEnd m rest)
                 )

remove :: (a -> Bool) -> [a] -> [a]
remove = \m -> (\n -> case n of
                      [] -> []
                      x : rest -> case (m x) of {True -> remove m rest;
                                                 False -> (x : remove m rest)}
               )

prefix :: Numb -> [a] -> [a]
prefix = \n -> (\m -> case n of
                      Z -> []
                      S n' -> case m of {[] -> []; x : rest -> (x : prefix n' rest)}
               )

reverse :: [a] -> [a]
reverse = \m -> case m of
                [] -> []
                x : rest -> (addToEnd x (reverse rest))

countStars :: RegExp -> Numb
countStars = \r -> case r of
                   Lit c -> Z
                   Alt r1 r2 -> add (countStars r1) (countStars r2)
                   Concat r1 r2 -> add (countStars r1) (countStars r2)
                   Star r -> add (S Z) (countStars r)
                   ZeroRE -> Z
                   OneRE -> Z

depth :: RegExp -> Numb
depth = \r -> case r of
              Lit c -> S Z
              Alt r1 r2 -> add (S Z) (bigger (depth r1) (depth r2))
              Concat r1 r2 -> add (S Z) (bigger (depth r1) (depth r2))
              Star r -> add (S Z) (depth r)
              ZeroRE -> S Z
              OneRE -> S Z

reToString :: RegExp -> [Char]
reToString = \r -> case r of
                   Lit c -> [c]
                   Alt r1 r2 -> "(" ++ (reToString r1) ++ "|" ++ (reToString r2) ++ ")"
                   Concat r1 r2 -> "(" ++ (reToString r1) ++ "." ++ (reToString r2) ++ ")"
                   Star r -> (reToString r) ++ "*"
                   ZeroRE -> "0"
                   OneRE -> "1"

