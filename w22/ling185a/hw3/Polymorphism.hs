module Polymorphism where

import Prelude hiding(map)

contains :: (a -> Bool) -> [a] -> Bool
contains = \f -> (\l -> case l of
                        [] -> False
                        x:rest -> case (f x) of
                                  True -> True
                                  False -> contains f rest
                 )

-- map (\x -> x + 1) [3,6,9]  ==>*  [4,7,10]
-- map (\x -> x > 10) [2,12,22]  ==>*  [False, True, True]
map :: (a -> b) -> [a] -> [b]
map = \f -> \l -> case l of
                  [] -> []
                  x:rest -> (f x):(map f rest)

-- containsElement 2 [2,5,7]  ==>*  True
-- containsElement 3 [2,5,7]  ==>*  False
containsElement :: (Eq a) => a -> [a] -> Bool
containsElement = \y -> \l -> case l of
                              [] -> False
                              x:rest -> case (x == y) of
                                        True -> True
                                        False -> containsElement y rest

data Fraction = Frac Int Int
                deriving Show

mult :: Fraction -> Fraction -> Fraction
mult = \f1 -> \f2 -> case (f1,f2) of
                     (Frac x1 y1, Frac x2 y2) -> Frac (x1 * x2) (y1 * y2)

equal :: Fraction -> Fraction -> Bool
equal = \f1 -> \f2 -> case (f1,f2) of
                      (Frac x1 y1, Frac x2 y2) -> (fromIntegral x1 / fromIntegral y1) == (fromIntegral x2 / fromIntegral y2)

instance Eq Fraction where
    x == y = (equal x y)

