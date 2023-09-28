-- Do not modify anything in this file!
module Def_with_verbose_names where

type ErrMsg = String    -- human-readable error messages
type EM = Either ErrMsg -- can be used directly as a Monad

type EGrammar = [ERule]
type ERule = (RLHS, ERHS)

type RLHS = (NTName, RKind, Maybe Type) -- rule LHS: non-terminal name, kind, and type
type NTName = String

data ERHS = -- extended rule right-hand side
    ESimple Simple -- 
  | ESeq [ERHS] HText
  | EBar ERHS ERHS
  | EOption ERHS
  | EMany ERHS
  | EPred ERHS HText
  | ENot ERHS
  deriving (Eq, Show, Read)

data Simple =
    SLit String -- literal string
  | SNTerm String -- non-terminal
  | SAnyChar -- any character
  | SChar Char -- character
  | SNot Simple -- not
  | SPred Simple HText -- predicate
  | SDummy -- dummy
  deriving (Eq,Show,Read)

data RKind = RPlain | RToken | RSep
  deriving (Eq,Show,Read)


data Action =
    AUser HText -- Haskell text taken directly from the input grammar, and may contain occurrences of _𝑖 variables. It will always be parenthesized when used.
  | AVar String -- 
  | ALam String Action -- constructor for wrapping the user actions in extra lambda-abstractions
  | AApp Action Action -- constructor for wrapping the user actions in extra applications
  | ACst String -- should be able to be inserted in any context (so it should include its own outer parentheses if needed), and should not use any _-variables.
  deriving (Eq,Show,Read)

type HText = String   -- Haskell text from user
type Type = Action -- 


type Grammar = [Rule]
type Rule = (RLHS, [([Simple]{-seq-}, Action)]{-alts-})
