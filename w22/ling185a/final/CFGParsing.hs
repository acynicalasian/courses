module CFGParsing where

import Data.List (intercalate, transpose)
import PrettyPrint.Boxes


-------------------------------------------------------------------------------
-- FSAs (for question 2)
-------------------------------------------------------------------------------
-- We're calling vowels "VW" to avoid clashes with the syntactic category V
data SegmentCV = C | VW deriving (Show, Eq)

type State = Int

type Automaton st sy = ([st], [sy], [st], [st], [(st,sy,st)])

fsa13 :: Automaton Int SegmentCV
fsa13 = ([1,2,3], [C,VW], [1], [1], [(1, VW, 1), (1, C, 2), (1, VW, 3), 
                                           (2, VW, 1), (2, VW, 3), 
                                           (3, C, 1)])

-- some helper functions

getDelta :: (Eq st, Eq sy) => Automaton st sy -> [(st,sy,st)]
getDelta (states, syms, i, f, delta) = delta

getGoalConfigs :: (Eq st, Eq sy) => Automaton st sy -> [(st,[sy])]
getGoalConfigs (states, syms, i, f, delta) = map (\x -> (x, [])) f

consumeFSA :: (Eq st, Eq sy) => [(st,sy,st)] -> (st,[sy]) -> [(st,[sy])]
consumeFSA delta (state, syms) =
  case syms of
  [] -> []
  x:rest -> let validtrans (q1,y,q2) = (q1 == state) && (x == y) in
            -- helper function that checks for possible transitions from current configuration
            let nextconfig (q1,y,q2) = (q2,rest) in
            map nextconfig (filter validtrans delta)

-------------------------------------------------------------------------------
-- CFGs
-------------------------------------------------------------------------------

-- NOTE: This version does not enforce Chomsky Normal Form
data RewriteRule nt t = NTRule nt [nt]  -- A -> B C D ...
                      | TRule nt t        -- A -> x
                      | NoRule            -- For starting configuration
                      deriving (Eq)

-- Corresponds to the definition in (3) on the Week 6 handout, 
-- except that we're assuming only one initial nonterminal
type CFG nt t = ([nt], [t], nt, [RewriteRule nt t])

-- For the sake of simplicity, we're only going to work with simple CFGs, not GCFGs,
-- but you should think about how we might extend these functions to semiring-based CFGs.
-- Notice that rules are represented in lists, rather than functions!

-----------------------------------------------------------
-- A few new helper functions

-- Returns the left-hand side of a rule
lhs :: RewriteRule nt t -> nt
lhs r = case r of {(NTRule x y) -> x; (TRule x y) -> x}

-- Returns the right-hand side of a terminal rule
rhsTRule :: RewriteRule nt t -> t
rhsTRule (TRule x y) = y

-- Returns the right-hand side of a nonterminal rule
rhsNTRule :: RewriteRule nt t -> [nt]
rhsNTRule (NTRule x ys) = ys


-------------------------------------------------------------------------------
-- PARSING
-------------------------------------------------------------------------------
-- Type for parser transitions

data Transition = NoTransition       -- For starting configurations
                | Shift              -- Bottom-up, left-corner
                | Reduce             -- Bottom-up
                | Predict            -- Top-down, left-corner
                | Match              -- Top-down, left-corner
                | Connect          -- Left-corner
                deriving (Show, Eq)

-- LC-PREDICT and LC-CONNECT are represented as Predict and Connect respectively.

-------------------------------------------------------------------------------
-- Types for configurations

type Config nt t = ( [Stack nt]  -- Stack of nonterminals.
                   , [t]         -- Input string still to be parsed.
                   )

data Stack nt = Bar nt     -- Shows with *, e.g. NP* and VP*.
              | NoBar nt
              deriving (Eq)

-- Bar is for left-corner parsing. For top-down and bottom-up, the
-- distinction between Bar and NoBar isn't necessary, so everything on the
-- stack will be NoBar, but it's simpler to have a single type for all three
-- parser types.

-------------------------------------------------------------------------------
-- Type for parse steps

data ParseStep nt t =
    ParseStep               -- (value constructor)
        Transition          -- Transition executed.
        (RewriteRule nt t)  -- Rule invoked.
        (Config nt t)       -- Resulting configuration.
    deriving (Eq)

-----------------------------------------------------------
-- Helper functions

getTransition :: ParseStep nt t -> Transition
getTransition (ParseStep trans rule config) = trans

getRule :: ParseStep nt t -> RewriteRule nt t
getRule (ParseStep trans rule config) = rule

getConfig :: ParseStep nt t -> Config nt t
getConfig (ParseStep trans rule config) = config

-------------------------------------------------------------------------------
-- Pretty-printing (don't worry about this part).
-------------------------------------------------------------------------------

instance (Show nt, Show t) => Show (RewriteRule nt t) where
    show (NTRule left right) =
        show left ++ " -> " ++ (unwords $ map show right)
    show (TRule left right) =
        show left ++ " -> " ++ show right
    show (NoRule) = "NoRule"

instance Show nt => Show (Stack nt) where
    show (Bar nt)   = show nt ++ "*"
    show (NoBar nt) = show nt

instance (Show nt, Show t) => Show (ParseStep nt t) where
    show     = intercalate " | " . listify
    showList = (++) . demarcate . makeTable . (map listify)

demarcate :: String -> String
demarcate str =
    "===== BEGIN PARSE =====\n" ++ str ++ "===== END PARSE =====\n"

listify :: (Show nt, Show t) => ParseStep nt t -> [String]
listify (ParseStep t r c) = [show t, show r, show c]

makeTable :: [[String]] -> String
makeTable rows =
    render $ hsep 3 left (map (vcat left . map text) (transpose rows))
