{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module Assignment08 where

import Control.Applicative(liftA, liftA2, liftA3)

import TreeGrammars

plainWords = ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]
whWords = ["who","what","why"]
qWords = ["Q"]

------------------------------------------------------
-- Some tiny helpers for writing trees more compactly

lf :: a -> Tree a
lf x = Node x []

mrg :: Tree String -> Tree String -> Tree String
mrg t1 t2 = Node "*" [t1,t2]

------------------------------------------------------

-- (1a)/(2a) `C John ate an apple yesterday'
tree_1a :: Tree String
tree_1a = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (1b)/(2b) `Q John ate what yesterday'
tree_1b :: Tree String
tree_1b = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

-- (3a) `Q John ate an apple yesterday'
tree_3a :: Tree String
tree_3a = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (3b) `C John ate what yesterday'
tree_3b :: Tree String
tree_3b = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

tree_13 :: Tree String
tree_13 =
    Node "*" [
        Node "Q" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "laughed" [],
                Node "**" [
                    Node "because" [],
                    Node "*" [
                        Node "Mary" [],
                        Node "*" [
                            Node "*" [Node "bought" [], Node "books" []],
                            Node "why" []
                        ]
                    ]
                ]
            ]
        ]
    ]

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

helper_count :: (Eq a) => a -> [Tree a] -> Int
helper_count s t = case t of
  [] -> 0
  x : rest -> count s x + helper_count s rest

count :: (Eq a) => a -> Tree a -> Int
count s t = let Node x y = t in
  (case (x == s) of { True -> 1; False -> 0 }) +
  case y of
    [] -> 0
    z : rest -> helper_count s y

leftEdge :: Tree a -> [a]
leftEdge t = case t of
  Node x y -> x : (case y of { [] -> []; z : rest -> leftEdge z })

addChar :: [[a]] -> [a] -> [[a]]
addChar a b = case b of
  [] -> []
  x : rest -> map (\a -> a ++ [x]) a ++ addChar a rest

allLists :: Int -> [a] -> [[a]]
allLists i s = case (i > 1) of
  True -> addChar (allLists (i - 1) s) s
  False -> case (i > 0) of
    True -> map (\s -> [s]) s
    False -> []

under :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> st -> Bool
under auto tree q =
  let (states, alpha, fin, trans) = auto in
    case tree of
      Node x [] -> elem ([], x, q) trans
      Node x y -> let conj qs = elem (qs, x, q) trans &&
                                  and (map (\(t, q) -> under auto t q) (zip y qs))
                  in
                    or (liftA conj (allLists (length y) states))

generates :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> Bool
generates auto t =
  let (states, alpha, fin, trans) = auto in
    or (map (\q -> under auto t q && elem q fin) states)

data State = Null | Wh | Q deriving (Show, Eq)
fsta_wh1 :: Automaton State String
fsta_wh1 =
  let
    states = [Null, Wh, Q]
    syms = plainWords ++ whWords ++ qWords ++ ["*"]
    f = [Null]
    trans =
      -- plainWords leaf nodes
      map (\w -> ([], w, Null)) plainWords ++
      -- whWords leaf nodes
      map (\w -> ([], w, Wh)) whWords ++
      -- qWords leaf nodes
      map (\w -> ([], w, Q)) qWords ++
      -- non-terminal nodes
      -- Q-states not being allowed to have children guarantees Q will c-command wh-words
      [ ([Null, Null], "*", Null), ([Wh, Null], "*", Wh),
        ([Null, Wh], "*", Wh),     ([Wh, Wh], "*", Wh),   ([Q, Wh], "*", Null),
                                   ([Wh, Q], "*", Null) ]
  in
    (states, syms, f, trans)

fsta_wh2 :: Automaton State String
fsta_wh2 =
  let
    states = [Null, Wh, Q]
    syms = plainWords ++ whWords ++ qWords ++ ["*","**"]
    f = [Null]
    trans = 
      -- plainWords leaf nodes
      map (\w -> ([], w, Null)) plainWords ++
      -- whWords leaf nodes
      map (\w -> ([], w, Wh)) whWords ++
      -- qWords leaf nodes
      map (\w -> ([], w, Q)) qWords ++
      -- non-terminal nodes
      -- Q-states not being allowed to have children guarantees Q will c-command wh-words
      [ ([Null, Null], "*", Null), ([Wh, Null], "*", Wh),
        ([Null, Wh], "*", Wh),     ([Wh, Wh], "*", Wh),   ([Q, Wh], "*", Null),
                                   ([Wh, Q], "*", Null) ] ++
      -- account for adjunct restrictions
      -- Do not allow non-[Null, Null] or non-[Q, Wh] format transitions with symbol "**"
      -- Enforces wh-licensing within adjuncts since children of "**" nodes still follow
      -- rules in transitions above
      [ ([Null, Null], "**", Null), ([Q, Wh], "**", Null), ([Wh, Q], "**", Null) ]
  in
    (states, syms, f, trans)
