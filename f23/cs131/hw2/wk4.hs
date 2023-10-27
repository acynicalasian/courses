clump :: (Eq a) => [a] -> [a] -> [[a]]
clump [] [] = []
clump [] (r:rs) = clump [r] rs
clump l [] = [l]
clump l r = if (head l == head r)
  then clump ((head r):l) (tail r)
  else [l] ++ clump [] r
  
encode [] = []
encode lst = map (\l -> (length l, head l)) lst
