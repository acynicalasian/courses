module Assignment03 where

-- Imports everything from the FiniteState module
import FiniteState

-- Another type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The word ``hello'' encoded as a snoc list of characters
sl :: SnocList Char
sl = ((((ESL ::: 'h') ::: 'e') ::: 'l') ::: 'l') ::: 'o'

-- Checks that all states and symbols mentioned in the transition 
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq a) => Automaton a -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

fsa_countVs :: Automaton SegmentCV
fsa_countVs = ([54, 73, 21, 38], [C, V], [54], [38], [(54, C, 54),
                                                      (54, V, 73),
                                                      (73, C, 73),
                                                      (73, V, 21),
                                                      (21, C, 21),
                                                      (21, V, 54),
                                                      (21, V, 38),
                                                      (38, C, 38)])

addToFront :: a -> SnocList a -> SnocList a
addToFront = \m -> (\n -> case n of
                          ESL -> ESL ::: m
                          rest ::: x -> (addToFront m rest) ::: x
                   )

toSnoc :: [a] -> SnocList a
toSnoc = \m -> case m of
               [] -> ESL
               x : rest -> (addToFront x (toSnoc rest))

forward :: (Eq a) => Automaton a -> SnocList a -> State -> Bool
forward m w q = let (states, syms, i, f, delta) = m in
                case w of
                ESL -> elem q i
                rest ::: x -> or (map (\qdec -> forward m rest qdec && elem (qdec, x, q) delta) states)

generates2 :: (Eq a) => Automaton a -> [a] -> Bool
generates2 m w = let (states, syms, i, f, delta) = m in
                 or (map (\qdec -> forward m (toSnoc w) qdec && elem qdec f) states)

fsa_twoCs :: Automaton SegmentCV
fsa_twoCs = ([1, 2, 3], [C, V], [1], [3], [(1, C, 1),
                                           (1, V, 1),
                                           (1, C, 2),
                                           (2, C, 2),
                                           (2, V, 2),
                                           (2, C, 3),
                                           (3, C, 3),
                                           (3, V, 3)])

fsa_thirdC :: Automaton SegmentCV
fsa_thirdC = ([1, 2, 3, 4], [C, V], [1], [4], [(1, C, 2),
                                               (1, V, 2),
                                               (2, C, 3),
                                               (2, V, 3),
                                               (3, C, 4),
                                               (4, C, 4),
                                               (4, V, 4)])

fsa_thirdlastC :: Automaton SegmentCV
fsa_thirdlastC = ([1, 2, 3, 4], [C, V], [1], [4], [(1, C, 1),
                                                   (1, V, 1),
                                                   (1, C, 2),
                                                   (2, C, 3),
                                                   (2, V, 3),
                                                   (3, C, 4),
                                                   (3, V, 4)])

fsa_oddEven :: Automaton SegmentCV
fsa_oddEven = ([1, 2, 3, 4], [C, V], [1], [2], [(1, C, 2),
                                                (2, C, 1),
                                                (1, V, 3),
                                                (3, V, 1),
                                                (2, V, 4),
                                                (4, V, 2),
                                                (3, C, 4),
                                                (4, C, 3)])

fsa_harmony :: Automaton SegmentPKIU
fsa_harmony = ([1, 2, 3, 4], [P, K, I, U, MB], [1], [4], [(1, P, 4),
                                                          (1, K, 4),
                                                          (1, I, 4),
                                                          (1, U, 4),
                                                          (1, MB, 4),
                                                          (1, P, 1),
                                                          (1, K, 1),
                                                          (1, MB, 1),
                                                          (1, I, 2),
                                                          (1, U, 3),
                                                          (2, P, 2),
                                                          (2, K, 2),
                                                          (2, I, 2),
                                                          (2, MB, 1),
                                                          (2, P, 4),
                                                          (2, K, 4),
                                                          (2, I, 4),
                                                          (2, MB, 4),
                                                          (3, P, 3),
                                                          (3, K, 3),
                                                          (3, U, 3),
                                                          (3, MB, 1),
                                                          (3, P, 4),
                                                          (3, K, 4),
                                                          (3, U, 4),
                                                          (3, MB, 4)])

fsa_MBU :: Automaton SegmentPKIU
fsa_MBU = ([1, 2], [P, K, I, U, MB], [1], [1], [(1, P, 1),
                                                (1, K, 1),
                                                (1, I, 1),
                                                (1, MB, 1),
                                                (1, MB, 2),
                                                (2, P, 2),
                                                (2, K, 2),
                                                (2, I, 2),
                                                (2, U, 2),
                                                (2, MB, 2),
                                                (2, P, 1),
                                                (2, K, 1),
                                                (2, I, 1),
                                                (2, U, 1),
                                                (2, MB, 1)])


fsa_adjacentMBU :: Automaton SegmentPKIU
fsa_adjacentMBU = ([1, 2], [P, K, I, U, MB], [1], [1], [(1, P, 1),
                                                        (1, K, 1),
                                                        (1, I, 1),
                                                        (1, MB, 1),
                                                        (1, MB, 2),
                                                        (2, P, 1),
                                                        (2, K, 1),
                                                        (2, I, 1),
                                                        (2, U, 1),
                                                        (2, MB, 1)])

requireCs :: Int -> Automaton SegmentCV
requireCs = \x -> case (x < 1) of
                  True -> ([1], [C, V], [1], [1], [(1, V, 1)])
                  False -> ([1 .. (x + 1)], [C, V], [1], [x + 1], (map (\x -> (x, V, x)) [1 .. x + 1]) ++ (map (\x -> (x, C, x + 1)) [1 .. x]))
