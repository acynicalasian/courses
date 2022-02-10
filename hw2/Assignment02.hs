module Assignment02 where

-- Imports everything from the Recursion module. 
import Recursion

-- Imports just a few things that we have seen from the standard Prelude module. 
-- (If there is no explicit 'import Prelude' line, then the entire Prelude 
-- module is imported. I'm restricting things here to a very bare-bones system.)
import Prelude((+), (-), (*), (<), (>), (++), not, Bool(..), Int, Show, Char)

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

equal :: Numb -> (Numb -> Bool)
equal = \m -> \n -> case m of
                    Z -> case n of
                         Z -> True
                         S n' -> False
                    S m' -> case n of
                            Z -> False
                            S n' -> equal m' n'

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

countStars :: RegExp a -> Numb
countStars = \r -> case r of
                   Lit c -> Z
                   Alt r1 r2 -> add (countStars r1) (countStars r2)
                   Concat r1 r2 -> add (countStars r1) (countStars r2)
                   Star r -> add (S Z) (countStars r)
                   ZeroRE -> Z
                   OneRE -> Z

depth :: RegExp a -> Numb
depth = \r -> case r of
              Lit c -> S Z
              Alt r1 r2 -> add (S Z) (bigger (depth r1) (depth r2))
              Concat r1 r2 -> add (S Z) (bigger (depth r1) (depth r2))
              Star r -> add (S Z) (depth r)
              ZeroRE -> S Z
              OneRE -> S Z
