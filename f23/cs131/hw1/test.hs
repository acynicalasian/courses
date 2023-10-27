f :: String -> String
f = (\y -> y ++ "poo")

g :: String -> String
g str = "hell nah"

test_curry :: Int -> (Int -> (Int -> Int))
test_curry a b c = a + b + c
