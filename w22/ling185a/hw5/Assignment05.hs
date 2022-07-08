{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lh :: (Semiring v) => v -> v -> v -> v
distrib_lh x y z = x &&& (y ||| z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

distrib_rh :: (Semiring v) => v -> v -> v -> v
distrib_rh x y z = (x &&& y) ||| (x &&& z)

expn :: (Semiring v) => v -> Numb -> v
expn base expo = case expo of
  Z -> gtrue
  S n' -> base &&& expn base n'

backward :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> st -> v
backward m w q = let (states, syms, i, f, delta) = m in
  case w of
    []  -> f q
    (x : rest) -> gen_or (map (\q1 -> delta (q, x, q1) &&& backward m rest q1) states)

f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f m w = let (states, syms, i, fin,  delta) = m in
  gen_or (map (\q -> i q &&& backward m w q) states)

addCosts :: Cost -> Cost -> Cost
addCosts x y = case x of
  TheInt m -> case y of
    TheInt n -> TheInt (m + n)
    Inf -> Inf
  Inf -> Inf  

minCost :: Cost -> Cost -> Cost
minCost x y = case x of
  TheInt m -> case y of
    TheInt n -> case (m > n) of
      True -> y
      False -> x
    Inf -> x
  Inf -> y

instance Semiring Cost where
  x &&& y = addCosts x y
  x ||| y = minCost x y
  gtrue = TheInt 0
  gfalse = Inf

helper_4H_concat :: [[a]] -> [[a]] -> [[a]]
helper_4H_concat arr1 arr2 = case arr1 of
  [] -> []
  x : rest -> case arr2 of
    [] -> []
    y : more -> (map (\y -> x ++ y) arr2) ++ helper_4H_concat rest arr2

instance Semiring [[a]] where
  x &&& y = helper_4H_concat x y
  x ||| y = x ++ y
  gtrue = [[]]
  gfalse = []

gfsa17 :: GenericAutomaton Int Char [[Char]]
gfsa17 = makeGFSA ([] :: [[Char]]) ([1, 2, 3], ['C', 'V'],
                                    [(1, [""])], [(1, [""])],
                                    [((1, 'V', 1), ["V"]),
                                     ((1, 'C', 2), ["C"]),
                                     ((1, 'V', 3), ["V"]),
                                     ((2, 'V', 1), ["V", "VV"]),
                                     ((2, 'V', 3), ["V", "VV"]),
                                     ((3, 'C', 1), [""])])
                                   
gfsa_flap :: GenericAutomaton Int Char [[Char]]
gfsa_flap = makeGFSA ([] :: [[Char]]) ([1, 2, 3], ['a', 'n', 't'],
                                       [(1, [""])],
                                       [(1, [""]), (2, [""]), (3, ["t"])],
                                       [((1, 'n', 1), ["n"]),
                                        ((1, 't', 1), ["t"]),
                                        ((1, 'a', 2), ["a"]),
                                        ((2, 'a', 2), ["a"]),
                                        ((2, 'n', 1), ["n"]),
                                        ((2, 't', 3), [""]),
                                        ((3, 'a', 2), ["ta", "Ta"]),
                                        ((3, 'n', 1), ["tn"]),
                                        ((3, 't', 1), ["tt"])])
