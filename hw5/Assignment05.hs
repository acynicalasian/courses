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

