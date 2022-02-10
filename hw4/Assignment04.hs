module Assignment04 where

import Prelude hiding (Either(..))

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

import FiniteStatePart2

---------------------------------------
-- Setup for section 1

type SLG sy = ([sy], [sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Show)

slg1 :: SLG SegmentCV
slg1 = ([C,V], [C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 2

data Either a b = First a | Second b deriving (Show,Eq)

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3)) 

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG slg str = let (alpha, i, f, pair) = slg in
                         case str of
                           [] -> False
                           x : [] -> elem x i && elem x f
                           x : sub -> elem x i && (case sub of
                                                     y : [] -> elem y f && elem (x, y) pair
                                                     y : z -> elem (x, y) pair && helper_1A slg (y : z)
                                                  )

helper_1A :: (Eq sy) => SLG sy -> [sy] -> Bool
helper_1A slg str = let (alpha, i, f, pair) = slg in
                      case str of
                        x : sub -> case sub of
                                     y : [] -> elem y f && elem (x, y) pair
                                     y : z -> elem (x, y) pair && helper_1A slg (y : z)


-- Helpers for slgToFSA
sy_checkPair :: (Eq sy) => sy -> [(sy, sy)] -> [(ConstructedState sy, sy, ConstructedState sy)]
sy_checkPair sym pair =
  case pair of
    [] -> []
    x : rest ->
      let (sy1, sy2) = x in
        case (sym == sy1) of
          True -> (StateForSymbol sym, sy2, StateForSymbol sy2) : sy_checkPair sym rest
          False -> sy_checkPair sym rest

sy_allCheck :: (Eq sy) => [sy] -> [(sy, sy)] -> [(ConstructedState sy, sy, ConstructedState sy)]
sy_allCheck sym pair =
  case sym of
    [] -> []
    x : rest ->
      (sy_checkPair x pair) ++ (sy_allCheck rest pair)


slgToFSA :: (Eq sy) => SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA slg  = let (slg_alpha, slg_i, slg_f, slg_pair) = slg in
  let
    auto_states = [ExtraState] ++ map StateForSymbol slg_alpha
    auto_alpha = slg_alpha
    auto_i = [ExtraState]
    auto_f = map StateForSymbol slg_f
    auto_trans = map (\sym -> (ExtraState, sym, StateForSymbol sym)) slg_i ++ sy_allCheck slg_alpha slg_pair
  in
    (auto_states, auto_alpha, auto_i, auto_f, auto_trans)

unionFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
unionFSAs auto1 auto2 = let (auto1_state, auto1_alpha, auto1_i, auto1_f, auto1_trans) = auto1 in
  let (auto2_state, auto2_alpha, auto2_i, auto2_f, auto2_trans) = auto2 in
    let
      out_state = map First auto1_state ++ map Second auto2_state
      out_alpha = nub (auto1_alpha ++ auto2_alpha)
      out_i = map First auto1_i ++ map Second auto2_i
      out_f = map First auto1_f ++ map Second auto2_f
      out_trans = map (\st -> let (a, b, c) = st in (First a, b, First c)) auto1_trans ++ map (\st -> let (a, b, c) = st in (Second a, b, Second c)) auto2_trans
    in
      (out_state, out_alpha, out_i, out_f, out_trans)

helper1E_midTrans :: st1 -> [st2] -> [(Either st1 st2, Maybe a, Either st1 st2)]
helper1E_midTrans st1 st2 = case st2 of
  x : rest -> (First st1, Nothing, Second x) : helper1E_midTrans st1 rest
  [] -> []
  
helper1E_addMids :: [st1] -> [st2] -> [(Either st1 st2, Maybe a, Either st1 st2)]
helper1E_addMids st1 st2 = case st1 of
  x : rest -> helper1E_midTrans x st2 ++ helper1E_addMids rest st2
  [] -> []
      
concatFSAs :: (Eq sy) => EpsAutomaton st1 sy -> EpsAutomaton st2 sy -> EpsAutomaton (Either st1 st2) sy
concatFSAs auto1 auto2 = let (auto1_state, auto1_alpha, auto1_i, auto1_f, auto1_trans) = auto1 in
  let (auto2_state, auto2_alpha, auto2_i, auto2_f, auto2_trans) = auto2 in
    let
      out_state = map First auto1_state ++ map Second auto2_state
      out_alpha = nub (auto1_alpha ++ auto2_alpha)
      out_i = map First auto1_i
      out_f = map Second auto2_f
      out_trans = map (\st -> let (a, b, c) = st in (First a, b, First c)) auto1_trans ++ map (\st -> let (a, b, c) = st in (Second a, b, Second c)) auto2_trans ++ helper1E_addMids auto1_f auto2_i
    in
      (out_state, out_alpha, out_i, out_f, out_trans)

helper1F_backTrans :: [st2] -> [st2] -> [(Either Int st2, Maybe a, Either Int st2)]
helper1F_backTrans st1 st2 = case st1 of
  x : rest -> map (\st -> (Second x, Nothing, Second st)) st2 ++ helper1F_backTrans rest st2
  [] -> []

starFSA :: EpsAutomaton st sy -> EpsAutomaton (Either Int st) sy
starFSA auto = let (auto_state, auto_alpha, auto_i, auto_f, auto_trans) = auto in
  let
    out_state = First 0 : map Second auto_state
    out_alpha = auto_alpha
    out_i = [(First 0)]
    out_f = First 0 : map Second auto_f
    out_trans = map (\st -> let (a, b, c) = st in (Second a, b, Second c)) auto_trans ++ map (\st -> (First 0, Nothing, Second st)) auto_i ++ helper1F_backTrans auto_f auto_i
  in
    (out_state, out_alpha, out_i, out_f, out_trans)

-- First x -> 1x
-- Second x -> 2x
flatten :: Either Int Int -> Int
flatten x = case x of
  First 0 -> 10
  Second 0 -> 20
  First n -> 10 ^ (floor (logBase 10 (fromIntegral n)) + 1) + n
  Second n -> 2 * (10 ^ (floor (logBase 10 (fromIntegral n)) + 1)) + n

mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates f auto = let (auto_state, auto_alpha, auto_i, auto_f, auto_trans) = auto in
  let
    out_state = map f auto_state
    out_alpha = auto_alpha
    out_i = map f auto_i
    out_f = map f auto_f
    out_trans = map (\st -> let (a, b, c) = st in (f a, b, f c)) auto_trans
  in
    (out_state, out_alpha, out_i, out_f, out_trans)

reToFSA :: (Eq sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA regex = case regex of
  Alt a b -> mapStates flatten (unionFSAs (reToFSA a) (reToFSA b))
  Lit a -> ([0, 1], a : [], [0], [1], [(0, Just a, 1)])
  Concat a b -> mapStates flatten (concatFSAs (reToFSA a) (reToFSA b))
  Star a -> mapStates flatten (starFSA (reToFSA a))
  ZeroRE -> ([0], [], [0], [], [])
  OneRE -> ([0], [], [0], [0], [(0, Nothing, 0)])


