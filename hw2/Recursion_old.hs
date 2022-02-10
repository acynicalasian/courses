module Recursion where

data Form = T | F | Neg Form | Cnj Form Form | Dsj Form Form deriving Show

f1 = Dsj (Neg T) (Cnj F T)

removeNegs = \form -> case form of
                      T -> T
                      F -> F
                      Neg phi -> removeNegs phi
                      Cnj phi psi -> Cnj (removeNegs phi) (removeNegs psi)
                      Dsj phi psi -> Dsj (removeNegs phi) (removeNegs psi)

-- The type 'Bool' is already defined for us like this:
-- data Bool = True | False deriving Show

-- Other pre-existing definitions we could have used:
-- not = \b -> case b of {True -> False; False -> True}

denotation = \form -> case form of
                      T -> True
                      F -> False
                      Neg phi -> case (denotation phi) of {True -> False; False -> True}
                      Cnj phi psi -> case (denotation phi) of {True -> denotation psi; False -> False}
                      Dsj phi psi -> case (denotation phi) of {True -> True; False -> denotation psi}

--------------------------------------

data Numb = Z | S Numb deriving Show

-- isDraw = \r -> case r of {Draw -> True; Win sh -> False}
-- f = \r -> case r of {Draw -> 1000; Win sh -> 100 + g sh}

isZero :: Numb -> Bool
isZero = \n -> case n of {Z -> True; S n' -> False}

subtractOneIfPossible :: Numb -> Numb
subtractOneIfPossible = \n -> case n of {Z -> Z; S n' -> n'}

lessThanTwo :: Numb -> Bool
lessThanTwo = \n -> case n of
                    Z -> True
                    S x -> case x of {Z -> True; S y -> False}

double :: Numb -> Numb
double = \n -> case n of {Z -> Z; S n' -> S (S (double n'))}

dbl :: Int -> Int
dbl = \n -> case (n <= 0) of {True -> 0; False -> 2 + dbl (n-1)}

isOdd :: Numb -> Bool
isOdd = \n -> case n of
              Z -> False
              S n' -> not (isOdd n')
                      -- case (isOdd n') of {True -> False; False -> True}

isEven :: Numb -> Bool
isEven = \n -> not (isOdd n)

add :: Numb -> (Numb -> Numb)
add = \n -> (\m -> case n of
                   Z -> m
                   S n' -> S ((add n') m)
            )

one = S Z
two = S one
three = S two
four = S three
five = S four

-- f x y    ===  (f x) y
-- add x y  ===  (add x) y

----------------------------------------------

data IntList = Empty | NonEmpty Int IntList deriving Show

myList = NonEmpty 5 (NonEmpty 7 (NonEmpty 2 Empty))

total = \l -> case l of
              Empty -> 0
              NonEmpty x rest -> x + total rest

otherTotal :: [Int] -> Int
otherTotal = \l -> case l of
                   [] -> 0
                   x : rest -> x + otherTotal rest

-- contains isOdd [two, four] ==>* False
-- contains isOdd [two, four, five] ==>* True
-- contains isEven [two, three] ==>* True
-- contains isEven [three] ==>* False
-- contains :: (Numb -> Bool) -> [Numb] -> Bool
-- contains :: (Int -> Bool) -> [Int] -> Bool
contains :: (a -> Bool) -> [a] -> Bool
contains = \f -> (\l -> case l of
                        [] -> False
                        x:rest -> case (f x) of
                                  True -> True
                                  False -> contains f rest
                 )

-- if b then x else y  ===  case b of {True -> x; False -> y}
-- x || y              ===  case x of {True -> True; False -> y}

----------------------------------------------

data RegExp = Lit Char | Alt RegExp RegExp | Concat RegExp RegExp 
            | Star RegExp | ZeroRE | OneRE
            deriving Show

re17a = Alt (Lit 'a') (Lit 'b')
re17b = Concat re17a (Lit 'c')
re17c = Star re17b

-- denotationRE = \re -> case re of
--                       Lit c ->
--                       Alt r1 r2 -> ... (denotationRE r1) ... (denotationRE r2) ...
