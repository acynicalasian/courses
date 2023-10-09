largest :: String -> String -> String
largest a b = case ((length a) >= (length b)) of
  True -> a
  False -> b

reflect :: Integer -> Integer
reflect 0 = 0
reflect num
  | num < 0 = (-1) + reflect (num + 1)
  | num > 0 = 1 + reflect (num - 1)

all_factors :: Integer -> [Integer]
all_factors x = [y | y <- [1..x], mod x y == 0]

perfect_numbers = [x | x <- [1..], (sum (init (all_factors x))) == x]

is_even_if :: Integer -> Bool
is_even_if x = if (x == 0) then True else
                 if (x == 1) then False else
                   is_even_if (x-2)

is_odd_if :: Integer -> Bool
is_odd_if x = is_even_if (x+1)

is_even_guards :: Integer -> Bool
is_even_guards x
  | x == 0 = True
  | x == 1 = False
  | otherwise = is_even_guards (x-2)

is_odd_guards :: Integer -> Bool
is_odd_guards x = is_even_guards (x+1)

is_even_pattern :: Integer -> Bool
is_even_pattern 0 = True
is_even_pattern 1 = False
is_even_pattern x = is_even_pattern (x-2)

is_odd_pattern :: Integer -> Bool
is_odd_pattern x = is_even_pattern (x+1)
