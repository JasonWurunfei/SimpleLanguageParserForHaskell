-- Put yor transformer implementation in this file
module TransformerImpl where

import Definitions
import Control.Monad.State

-- State monad to keep track of the converted grammar
-- (referred from Exam 2021 sample solution)
type SRules a = StateT [Rule] EM a

-- Report error (referred from Exam 2021 sample solution)
err :: String -> SRules a
err s = lift $ Left s

-- Get all the names of the non-terminals
getNTNames :: SRules [NTName]
getNTNames = gets (map (\((nt, _, _), _) -> nt))

-- Get the name of the non-terminal that is 
-- currently being transformed
currentNTName :: SRules NTName
currentNTName = do
  names <- getNTNames
  case names of
      [] -> err "no non-terminal name defined"
      _ ->
          return $ last names

-- Generate a new non-terminal name
freshNTName :: SRules NTName
freshNTName = do
  current <- currentNTName
  return $ current ++ "'"


-- Convert a EGrammar to a Grammar
-- It perform basic error checking:
-- 1. check that all nonterminals referenced in the grammar
--    are actually defined, but only once.
-- 2. the grammar should always contain a token-separation 
--    definition (i.e non-terminal with `_` as name)
convert :: EGrammar -> EM Grammar
convert [] = Left "empty grammar" -- just to get rid of the warning
convert eg = do
  -- convert the grammar
  (_, rs) <- runStateT (mapM convertRule eg) []
  -- check that the grammar contains a token-separation definition
  let nts = map (\((nt, _, _), _) -> nt) rs
  if "_" `elem` nts then
    return rs
  else
    Left "Grammar does not contain a token-separation definition"

-- Convert a rule
convertRule :: ERule -> SRules ()
convertRule ((nt, kind, ty), rhs) = do
  -- check if the non-terminal name is unique
  nts <- getNTNames
  if nt `elem` nts then
    err $ "non-terminal " ++ nt ++ " is defined more than once"
  else do
    -- insert the left hand side of the rule temporarily so
    -- that the current non-terminal name is set.
    rs <- get
    put $ rs ++ [((nt, kind, ty), [])]
    -- convert the right-hand side
    rhs' <- convertRHS rhs -- may insert new rules in the grammar
    -- remove the temporary rule
    -- find the rule that was inserted
    rs' <- get
    let (rs_1, rs_2) = break (\((nt', _, _), _) -> nt == nt') rs'
    -- remove the temporarily rule and insert the new rule
    put $ rs_1 ++ tail rs_2 ++ [((nt, kind, ty), rhs')]

-- Extract all the alternative definitions for a non-terminal RHS
extractAlts :: ERHS -> [ERHS]
extractAlts (EBar e1 e2) = extractAlts e1 ++ extractAlts e2
extractAlts e = [e]

-- Convert right-hand side
convertRHS :: ERHS -> SRules [([Simple], Action)]
convertRHS rhs = do
  -- alternative are treated differently than choices
  let alts  = extractAlts rhs
  alts' <- mapM convertRHS' alts
  return $ concat alts'

-- Convert right-hand side without considering alternatives
convertRHS' :: ERHS -> SRules [([Simple], Action)]
convertRHS' (ESimple (SNTerm s))  = return [([SNTerm s], AUser "_1")]
convertRHS' (ESimple (SLit s))    = return [([SLit s], AUser "()")]
convertRHS' (ESimple (SChar c))   = return [([SChar c], AUser "()")]
convertRHS' (ESimple SAnyChar)    = return [([SAnyChar], AUser "()")]
convertRHS' (ESimple (SNot s))    = return [([SNot s], AUser "()")]
convertRHS' (ESimple (SPred s h)) = return [([SPred s h], AUser "()")]
convertRHS' (ESimple SDummy)      = return [([SDummy], AUser "undefined")]
convertRHS' (ESeq es h) = do
  es' <- mapM convertRHS' es
  let ss = concatMap (fst.head) es'
  return [(ss, AUser h)]
convertRHS' (EBar e1 e2) = do
  fresh <- freshNTName
  let nt = SNTerm fresh
  rhs <- convertRHS (EBar e1 e2)
  rs <- get
  put $ rs ++ [((fresh, RPlain, Nothing), rhs)]
  return [([nt], AUser "_1")]
convertRHS' (EOption e) = do
  fresh <- freshNTName
  let nt = SNTerm fresh
  rhs <- convertRHS e
  rs <- get
  put $ rs ++ [((fresh, RPlain, Nothing), rhs)]
  return [([nt], AUser "_1"), ([], AUser "()")]
convertRHS' _ = err "not implemented"




lre :: Grammar -> EM Grammar
lre = undefined

lfactor :: Grammar -> EM Grammar
lfactor = undefined
