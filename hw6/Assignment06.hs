{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Assignment06 where

import Control.Applicative(liftA, liftA2, liftA3)

import ContextFree

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

tree1 :: Tree Cat String
tree1 = NonLeaf VP (NonLeaf VP (Leaf V "watches") (Leaf NP "spies"))
                   (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes"))

tree2 :: Tree Cat String
tree2 = NonLeaf VP (Leaf V "watches")
                   (NonLeaf NP (Leaf NP "spies") (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes")))
------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

getNT :: Tree nt t -> nt
getNT tr = case tr of
  Leaf x y -> x
  NonLeaf x y z -> x
  
treeToDeriv :: Tree nt t -> [RewriteRule nt t]
treeToDeriv tr = case tr of
  Leaf x y -> [TRule x y]
  NonLeaf x y z -> [NTRule x (getNT y, getNT z)] ++ treeToDeriv y ++ treeToDeriv z

f :: (Ord nt, Ord t, Semiring a) => GenericCFG nt t a -> [t] -> a
f m w = let (nt, t, i, r) = m in
  gen_or (map (\n -> i n &&& fastInside m w n) nt)

-- In case anyone sees this pointless comment, I've finally come to the point
-- where I genuinely, *genuinely* have no clue what the hell any of my code
-- is doing anymore. I'm just basing this implementation on how inside is
-- implemented, substituting variables whenever needed
outside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> ([t],[t]) -> nt -> v
outside cfg str n =
  let (nt, t, i, r) = cfg in
    case str of
      ([], []) -> i n
      -- What kind of magic nonsense is going on here? Seems to work but????
      (x, y) -> let conj1 i1 p1 rd = outside cfg (x, drop i1 y) p1 &&&
                                     r (NTRule p1 (n, rd)) &&&
                                     inside cfg y rd
                in
                  let conj2 i2 p2 ld = outside cfg (take i2 x, y) p2 &&&
                                       r (NTRule p2 (ld, n)) &&&
                                       inside cfg (drop i2 x) ld
                  in
                    (gen_or (liftA3 conj1 [1 .. (length y)] nt nt)) |||
                    (gen_or (liftA3 conj2 [0 .. (length x - 1)] nt nt))
                  
