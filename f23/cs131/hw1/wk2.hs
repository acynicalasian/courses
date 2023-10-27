compress :: String -> String
compress l = foldr (\ch -> \acc -> case acc of
                       [] -> [ch]
                       s -> if (head s == ch) then s else ch:s) "" l

rev :: [a] -> [a]
rev l = case l of
  [] -> []
  m:rest -> []
