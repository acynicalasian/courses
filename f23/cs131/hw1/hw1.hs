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
