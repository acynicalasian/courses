module Recursion where

----------------------------------------------------
-- 1. Propositional formulas

data Form = T | F | Neg Form | Cnj Form Form | Dsj Form Form deriving Show

f1 :: Form
f1 = Dsj (Neg T) (Cnj F T)

-- removeNegs = \form -> case form of {T -> T; F -> F; ....}
removeNegs :: Form -> Form
removeNegs = \form -> case form of
                      T -> T
                      F -> F
                      Neg phi -> removeNegs phi
                      Cnj phi psi -> Cnj (removeNegs phi) (removeNegs psi)
                      Dsj phi psi -> Dsj (removeNegs phi) (removeNegs psi)

-- The type `Bool' is defined under the hood something like this:
-- data Bool = False | True deriving Show

-- The function `not' is defined under the hood something like this:
-- not = \b -> case b of {True -> False; False -> True}

denotation :: Form -> Bool
denotation = \form -> case form of
                      T -> True
                      F -> False
                      Neg phi -> not (denotation phi)  -- case (denotation phi) of {True -> False; False -> True}
                      Cnj phi psi -> case (denotation phi) of {True -> denotation psi; False -> False}
                      Dsj phi psi -> case (denotation phi) of {True -> True; False -> denotation psi}

----------------------------------------------------
-- 2. A very simple recursive type

data Numb = Z | S Numb deriving Show

one = S Z
two = S one
three = S two
four = S three
five = S four

-- A non-recursive function on the Numb type. Notice how non-recursive 
-- functions like this are insensitive to distinctions between numbers 
-- that one can only see by looking beyond a fixed depth. (For example, 
-- this lessThanTwo function doesn't ``see'' the distinction between 
-- three and four.)
lessThanTwo :: Numb -> Bool
lessThanTwo = \n -> case n of
                    Z -> True
                    S n' -> case n' of {Z -> True; S n'' -> False}

-- Our first recursive function. 
-- This is equivalent to the function in (11) on the handout.
double :: Numb -> Numb
double = \n -> case n of
               Z -> Z
               S n' -> S (S (double n'))

-- A doubling function that works the same way but on built-in integers. The idea that 
-- recursive calls apply to ``sub-parts'' of the argument doesn't come out so clearly this way.
double' :: Int -> Int
double' = \n -> case (n <= 0) of {True -> 0; False -> 2 + (double' (n-1))}

-- Notice how this function says, in effect, that Z gets ``converted'' to `False' 
-- and that S gets converted to `not'.
-- For example, `isOdd (S (S (S Z)))' is worked out as `(not (not (not False)))'.
isOdd :: Numb -> Bool
isOdd = \n -> case n of
              Z -> False
              S n' -> case (isOdd n') of {True -> False; False -> True}
                      -- (\b -> case b of {True -> False; False -> True}) (isOdd n')
                      -- not (isOdd n')

-- This is written in a way that makes it clear that this is a function 
-- which takes a number as an argument and returns a function
add :: Numb -> (Numb -> Numb)
add = \n -> case n of
            Z    -> \m -> m
            S n' -> \m -> S ((add n') m)   -- or: (add n') (S m)

-- This version is entirely equivalent to `add' but makes it look a bit 
-- more like a ``function that takes two arguments''
otherAdd :: Numb -> (Numb -> Numb)
otherAdd = \n -> \m -> case n of
                       Z    -> m
                       S n' -> S ((otherAdd n') m)   -- or: (add n') (S m)


-- Some functions you'll be discussing in section, and which will be helpful for the HW
lessThanOrEq :: Numb -> (Numb -> Bool)
lessThanOrEq = \n -> \m -> case n of
                Z -> True
                S n' -> case m of
                        Z -> False
                        S m' -> (lessThanOrEq n') m'


bigger :: Numb -> (Numb -> Numb)
bigger = \n -> \m -> case n of
                      Z -> m
                      S n' -> case m of
                              Z -> n
                              S m' -> S ((bigger n') m')

----------------------------------------------------
-- 3. Another recursive type: lists/strings

data IntList = Empty | NonEmpty Int IntList deriving Show

-- Notice this definition, which gives a name to a list, is not interestingly 
-- different from all the other surrounding definitions, which give names to functions.
myList :: IntList
myList = NonEmpty 5 (NonEmpty 7 (NonEmpty 2 Empty))

-- Two versions of the 'total' function from the handout
total :: IntList -> Int
total = \l -> case l of
              Empty -> 0
              NonEmpty x rest -> x + total rest

otherTotal :: [Int] -> Int
otherTotal = \l -> case l of
                   [] -> 0
                   x:rest -> x + otherTotal rest

----------------------------------------------------
-- 4. Polymorphism

data Shape = Rock | Paper | Scissors deriving Show

containsRock :: [Shape] -> Bool
containsRock = \l -> case l of
                     [] -> False
                     x : rest -> case x of
                                 Rock -> True
                                 Paper -> containsRock rest
                                 Scissors -> containsRock rest

-- This function has a ``completely flexible'' type: the `a' 
-- variable in its type is entirely unrestricted, because 
-- the function's implementation doesn't make any assumptions 
-- about what can be done with elements of the list.
size :: [a] -> Numb
size = \l -> case l of
             [] -> Z
             x : rest -> S (size rest)

{-
-- Now suppose we wanted to write a function for checking whether something 
-- is an element of a list, which would work with various types, like this:
--      elementOf Rock [Rock,Paper] ==>* True
--      elementOf 3 [1,2,4,5] ==>* False
-- Based on containsRock above, we might start with something like the 
-- following. But we get stuck, because we don't know anything about the type `a' and 
-- so we don't know how to check whether x counts as the same as y.
elementOf :: a -> ([a] -> Bool)
elementOf = \x -> \l -> case l of
                        [] -> False
                        y : rest -> case (??? x ??? y ???) of
                                    ??? -> True
                                    ??? -> elementOf x rest
-}

{-
-- Here is one solution that works: we can ``outsource'' the equality-checking to the 
-- caller of the elementOf function. (This would allow the caller to, for example, 
-- provide equivalence-modulo-ten as the function to be use, if they wished.)
elementOf :: (a -> a -> Bool) -> (a -> ([a] -> Bool))
elementOf = \isEqual -> \x -> \l -> case l of
                                    [] -> False
                                    y : rest -> case (isEqual x y) of
                                                True -> True
                                                False -> elementOf isEqual x rest
-}

-- Haskell gives us a more convenient way of doing something which is 
-- equivalent (under the hood) to the outsourcing solution. The following 
-- type signature imposes a requirement that the type `a' must be one of 
-- the types in the `Eq' class; these are types that have an equality function 
-- associated with them, which is what gets invoked when we write `x == y'.

elementOf :: (Eq a) => (a -> ([a] -> Bool))
elementOf = \x -> \l -> case l of
                        [] -> False
                        y : rest -> case (x == y) of
                                    True -> True
                                    False -> elementOf x rest

-- If we included `Eq' in the `deriving' list when we first declared our Shape type,
-- then the ``obvious'' equality-checking function would be defined for Shapes: 
-- data Shape = Rock | Paper | Scissors deriving (Show,Eq)

-- Another function on lists that you'll see in section
contains :: (a -> Bool) -> [a] -> Bool
contains = \f -> \l -> case l of
                        [] -> False
                        x : rest -> case (f x) of
                                    True -> True
                                    False -> contains f rest

----------------------------------------------------
-- 5. Regular expressions
-- Our type for regular expressions, following the definition in the handout
data RegExp a = Lit a | Alt (RegExp a) (RegExp a) | Concat (RegExp a) (RegExp a) 
              | Star (RegExp a) | ZeroRE | OneRE
              deriving (Eq,Show)

-- Some example regular expressions, as in (27) from the handout
re_27a = Alt (Lit 'a') (Lit 'b')
re_27b = Concat re_27a (Lit 'c')
re_27c = Star re_27b
