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

outside :: (Ord nt, Ord t, Semiring v) => GenericCFG nt t v -> ([t],[t]) -> nt -> v
outside cfg str n =
  let (nt, t, i, r) = cfg in
    case str of
      ([], []) -> i n
      (x : [], y : []) -> 
