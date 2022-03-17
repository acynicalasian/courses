module FinalProject01 where

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List

import CFGParsing

bottomUp :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
bottomUp cfg input =
  let (nts, ts, start, rules) = cfg in
  let startingConfig = ([], input) in
  let goalConfig = ([NoBar start], []) in
  parser [shift, reduce] rules startingConfig goalConfig
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Type for syntactic categories. 
-- I've moved this to "below the line" (from the CFGParsing module) so you can modify it.
data Cat = S | NP | VP | PP | N | V | P | WHILE | POSS | ORC | SRC
             | THAT | OR | AND | D | A deriving (Show, Eq, Ord)

-- Some example grammars

-- From (12) on the Week 6 handout
cfg12 :: CFG Cat String
cfg12 = ([VP,NP,PP,V,P], ["watches","spies","telescopes","with"], 
         VP, 
         [(NTRule VP [V,NP]), (NTRule NP [NP,PP]), (NTRule PP [P,NP]),
          (NTRule VP [VP,PP]),  (TRule NP "telescopes"),
          (TRule VP "watches"), (TRule NP "watches"), (TRule P "with"), 
          (TRule VP "spies"), (TRule NP "spies"), (TRule V "watches")
          ]
        )

-- From (4) on the Week 7 handout
cfg4 :: CFG Cat String
cfg4 = ([S,NP,VP,PP,N,V,P,D,ORC,SRC,THAT,POSS,WHILE], 
        ["baby","boy","actor","spouse","boss","award", "Mary","John","met","saw","won","the","on","in","with","that","'s","while"], 
        S,
        [(NTRule S [NP,VP]), (NTRule S [WHILE,S,S]),
         (NTRule NP [NP,POSS,N]), (NTRule NP [D,N,PP,SRC,ORC]), (NTRule NP [N,PP,SRC,ORC]),
         (NTRule NP [D,N,SRC,ORC]), (NTRule NP [D,N,PP,SRC]), (NTRule NP [D,N,PP,ORC]),
         (NTRule NP [D,N,PP]), (NTRule NP [D,N,SRC]), (NTRule NP [D,N,ORC]),
         (NTRule NP [N,PP,SRC]), (NTRule NP [N,PP,ORC]), (NTRule NP [N,SRC,ORC]),
         (NTRule NP [D,N]), (NTRule NP [N,PP]), (NTRule NP [N,SRC]), (NTRule NP [N,ORC]),
         (NTRule VP [V,NP,PP]), (NTRule VP [V,NP]), (NTRule VP [V,PP]), (NTRule VP [V]),
         (NTRule PP [P,NP]), (NTRule SRC [THAT,VP]), (NTRule ORC [NP,V]),
         (TRule N "baby"), (TRule N "boy"), (TRule N "actor"), (TRule N "spouse"), (TRule N "boss"), (TRule N "award"),
         (TRule NP "Mary"), (TRule NP "John"),
         (TRule V "met"), (TRule V "saw"), (TRule V "won"), 
         (TRule D "the"), (TRule P "on"), (TRule P "in"), (TRule P "with"),
         (TRule THAT "that"), (TRule POSS "'s"), (TRule WHILE "while")
        ]
       )

-- These functions are placeholders to work with 'bottomUp' in Part 1.3. 
-- You should replace 'undefined' in these functions with your own code.

shift :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shift rule conf = case rule of
  [] -> []
  rw : rest -> case rw of
    NTRule u v -> shift rest conf
    TRule a x -> let (conf_l, conf_r) = conf in
      case conf_r of
        [] -> []
        foo -> case (x == head conf_r) of
          True -> (ParseStep Shift rw (conf_l ++ [NoBar a], drop 1 conf_r)) : shift rest conf
          False -> shift rest conf

-- Helper for reduce; checking left side of Config for match w/right side of NTRule
-- If Config's left side is too long, add the head of the left side to front and
-- recall ntr_match
ntr_reduce :: (Eq nt) => [nt] -> [Stack nt] -> [Stack nt] -> ([Stack nt], Bool)
ntr_reduce r front conf = case conf of
  [] -> ([], False)
  l : rest -> case ((map (\n -> NoBar n) r) == conf) of
    True -> (front, True)
    False -> ntr_reduce r (front ++ [l]) rest

reduce :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
reduce rule conf = case rule of
  [] -> []
  rw : rest -> case rw of
    TRule u v -> reduce rest conf
    NTRule a r -> let (conf_l, conf_r) = conf in
      case conf_l of
        [] -> []
        foo -> let (front, check) = ntr_reduce r [] conf_l in
          case check of
            True -> (ParseStep Reduce rw (front ++ [NoBar a], conf_r)) : reduce rest conf
            False -> reduce rest conf

stepBranch :: (Eq nt, Eq t)
  => ([RewriteRule nt t] -> Config nt t -> [ParseStep nt t]) -- step to take
  -> [RewriteRule nt t] -- CFG rules
  -> [ParseStep nt t] -- current branch
  -> Config nt t -- goal config
  -> [[ParseStep nt t]]
stepBranch step rules branch goal = let lastConf = getConfig (last branch) in
  let check = step rules lastConf in
    case check of
      [] -> case (lastConf == goal) of
        True -> [branch]
        False -> []
      pass -> map (\p -> branch ++ [p]) pass

stepBranchAll :: (Eq nt, Eq t)
  => [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]] -- steps to take
  -> [RewriteRule nt t] -- CFG rules
  -> [ParseStep nt t] -- current branch
  -> Config nt t -- goal config
  -> [[ParseStep nt t]]
stepBranchAll steps rules branch goal = case steps of
  [] -> []
  s : rest -> stepBranch s rules branch goal ++ stepBranchAll rest rules branch goal

splitBranches :: (Eq nt, Eq t)
  => [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]] -- steps to take
  -> [RewriteRule nt t] -- CFG rules
  -> [[ParseStep nt t]] -- branches
  -> Config nt t -- goal config
  -> [[ParseStep nt t]]
splitBranches steps rules branches goal = case branches of
  [] -> []
  b : rest -> (stepBranchAll steps rules b goal) ++ (splitBranches steps rules rest goal)

genBranches :: (Eq nt, Eq t)
  => [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]] -- steps to take
  -> [RewriteRule nt t] -- CFG rules
  -> [[ParseStep nt t]] -- branches
  -> Config nt t -- goal config
  -> [[ParseStep nt t]]
genBranches steps rules branches goal =
  let cmp = and (map (\r -> getConfig (last r) == goal) branches) in
    case branches of
      [] -> []
      foo -> case cmp of        
        True -> branches
        False -> genBranches steps rules (splitBranches steps rules branches goal) goal

parser :: (Eq nt, Eq t)
       => [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]]
          -- ^ List of transition steps. ^
       -> [RewriteRule nt t]  -- Rules from the CFG.
       -> Config nt t         -- Starting configuration.
       -> Config nt t         -- Goal configuration.
       -> [[ParseStep nt t]]  -- List of possible parses.
parser steps rules start goal = genBranches steps rules [[ParseStep NoTransition NoRule start]] goal

match :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
match rule conf = case rule of
  [] -> []
  rw : rest -> case rw of
    NTRule u v -> match rest conf
    TRule a x -> let (conf_l, conf_r) = conf in
      case conf_l of
        [] -> []
        foo -> case conf_r of
          [] -> []
          fu -> case (NoBar a == head conf_l && x == head conf_r) of
            True -> (ParseStep Match rw (drop 1 conf_l, drop 1 conf_r)) : match rest conf
            False -> match rest conf

predict :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
predict rule conf = case rule of
  [] -> []
  rw : rest -> case rw of
    TRule u v -> predict rest conf
    NTRule a r -> let (conf_l, conf_r) = conf in
      case conf_l of
        [] -> []
        foo -> case (NoBar a == head conf_l) of
          True -> let s = map (\r -> NoBar r) r in
            case (length (s ++ conf_l) - 1 > length conf_r) of
              True -> predict rest conf
              False -> (ParseStep Predict rw (s ++ drop 1 conf_l, conf_r)) : predict rest conf
          False -> predict rest conf

topDown :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
topDown cfg input =
  let
    (_, _, nt_init, rules) = cfg
    start = ([NoBar nt_init], input)
    goal = ([], [])
  in
    parser [match, predict] rules start goal

matchLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
matchLC rule conf = case rule of
  [] -> []
  rw : rest -> case rw of
    NTRule u v -> matchLC rest conf
    TRule a x -> let (conf_l, conf_r) = conf in
      case conf_l of
        [] -> []
        foo -> case conf_r of
          [] -> []
          fu -> case (Bar a == head conf_l && x == head conf_r) of
            True -> (ParseStep Match rw (drop 1 conf_l, drop 1 conf_r)) : matchLC rest conf
            False -> matchLC rest conf

shiftLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shiftLC rule conf = case rule of
  [] -> []
  rw : rest -> case rw of
    NTRule u v -> shiftLC rest conf
    TRule a x -> let (conf_l, conf_r) = conf in
      case conf_r of
        [] -> []
        foo -> case (x == head conf_r) of
          True -> (ParseStep Shift rw ([NoBar a] ++ conf_l, drop 1 conf_r)) : shiftLC rest conf
          False -> shiftLC rest conf

countBar :: [Stack nt] -> Int
countBar conf = case conf of
  [] -> 0
  x : rest -> case x of
    NoBar a -> countBar rest
    Bar b -> 1 + countBar rest

predictLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
predictLC rule conf = case rule of
  [] -> []
  rw : rest -> case rw of
    TRule u v -> predictLC rest conf
    NTRule a r -> let (conf_l, conf_r) = conf in
      case conf_l of
        [] -> []
        foo -> case (NoBar (head r) == head conf_l) of
          True -> let s = map (\r -> Bar r) (drop 1 r) in
            case (countBar s + countBar conf_l > length conf_r) of
              True -> predictLC rest conf
              False -> (ParseStep Predict rw (s ++ [NoBar a] ++ drop 1 conf_l, conf_r)) : predictLC rest conf
          False -> predictLC rest conf

connectLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
connectLC rule conf = let (conf_l, conf_r) = conf in
  case (length conf_l >= 2) of
    False -> []
    True -> case rule of
      [] -> []
      rw : rest -> case rw of
        TRule u v -> connectLC rest conf
        NTRule a r -> case (NoBar (head r) == head conf_l && Bar a == head (drop 1 conf_l)) of
          True -> let s = map (\r -> Bar r) (drop 1 r) in
            case (countBar (s ++ drop 2 conf_l) > length conf_r) of
              True -> connectLC rest conf
              False -> (ParseStep Connect rw (s ++ drop 2 conf_l, conf_r)) : connectLC rest conf
          False -> connectLC rest conf

leftCorner :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
leftCorner cfg input =
  let
    (_, _, nt_init, rules) = cfg
    start = ([Bar nt_init], input)
    goal = ([], [])
  in
    parser [matchLC, shiftLC, predictLC, connectLC] rules start goal

test :: (Eq nt, Eq t) => CFG nt t -> [t] -> [ParseStep nt t]
test cfg input = let (_, _, _, rules) = cfg in
  shift rules ([], input)

test2 cfg input = let (_, _, start, rules) = cfg in
  genBranches [shift, reduce] rules [[ParseStep NoTransition NoRule ([], input)]] ([NoBar start], [])

-- stepBranch works
test3 cfg input = let (_, _, start, rules) = cfg in
  genBranches [shift, reduce] rules [[ParseStep NoTransition NoRule ([], input)]] ([NoBar start], [])
