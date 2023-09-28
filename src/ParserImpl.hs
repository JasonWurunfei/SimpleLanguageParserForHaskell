-- Put yor parser implementation in this file
module ParserImpl where

import Definitions
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char

type Parser = ReadP

parseSpec :: String -> EM (String, EGrammar)
parseSpec s = case readP_to_S pSpec s of
                  [] -> Left "Parse error"
                  [(x, "")] -> Right x
                  _ -> Left "Ambiguous"

-- 
------------------------- utility parsers -----------------------
--
-- comment parser
comment :: Parser ()
comment = do string "--"; munch (/= '\n'); commentEnd
  where
    commentEnd = eof <|> do char '\n'; return ()

-- skip white space(s)
skipws :: Parser ()
skipws = do skipSpaces; comment; skipws
            <++ skipSpaces

-- skip white space(s) after token parser 
-- (referred from Exam 2021 sample solution)
lexeme :: Parser a -> Parser a
lexeme p = do a <- p; skipws; return a

-- expect specific symbolic token 
-- (referred from Exam 2021 sample solution)
symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- 
------------------------- Token Parsers -------------------------
-- 
-- preamble: Any sequence of characters, terminated by '---' on a single line.
pPreamble :: Parser String
pPreamble = lexeme $
  do preambleEnd []
    where
      preambleEnd :: String -> Parser String
      preambleEnd s = do string "---\n"; return s
                        <|> do c <- get; preambleEnd (s ++ [c])

-- name: Any sequence of (Unicode) letters, digits, and underscores, 
-- starting with a letter.
pName :: Parser String
pName = lexeme $
  do
    c <- satisfy isAlpha
    cs <- munch (\c -> isAlphaNum c || c == '_')
    return (c:cs)

-- htext: Any sequence of arbitrary characters, including any leading whitespace.
-- Characters { and } to be included in the sequence must be written as {{ and }},
-- respectively. When following an opening {, the htext must not start with a : or ?.
htext :: Parser String
htext = htext' [] -- include leading whitespace hence not using lexeme
  where
    htext' :: String -> Parser String
    htext' s =
      do
        c1 <- get
        if c1 == '{' || c1 == '}' then
          do
            c2 <- get
            if c1 == '{' && c2 == '{' then
              htext' (s ++ ['{'])
            else if c1 == '}' && c2 == '}' then
              htext' (s ++ ['}'])
            else
              pfail
        else
          htext' (s ++ [c1])
      <++
      return s

-- tokLit: Any non-empty sequence of printable characters, enclosed in 
-- double-quotes. If the sequence is itself to contain a double-quote, 
-- it must be written as "".
pTokLit :: Parser String
pTokLit = lexeme $
  do
    char '"'
    s <- tokLit' []
    char '"'
    return s
  where
    tokLit' :: String -> Parser String
    tokLit' s =
      do
        c1 <- get
        if c1 == '"' then
          do
            c2 <- get
            if c2 == '"' then
              tokLit' (s ++ ['"'])
            else
              pfail
        else
          tokLit' (s ++ [c1])
      <++
      do
        if null s then -- empty string
          pfail
        else
          return s

-- charLit: Any printable character (including '), enclosed in single-quotes
pCharLit :: Parser Char
pCharLit = lexeme $
  do
    char '\''
    c <- get
    char '\''
    return c

-- 
------------------------- Grammar Parsers -----------------------
-- 
-- Spec ::= preamble ERules.
pSpec :: Parser (String, EGrammar)
pSpec = do
  s <- pPreamble
  rs <- pERules
  return (s, rs)

-- ERules ::= ERule | ERule ERules.
-- After left factorization: 
--  ERules ::= ERule ERules'.
--  ERules' ::= ERule ERules' | ε.
pERules :: Parser EGrammar
pERules = do
  r <- pERule
  rs <- pERules'
  return (r:rs)
  where
    pERules' :: Parser EGrammar
    pERules' = do
      r <- pERule
      rs <- pERules'
      return (r:rs)
      <++
      return []

-- ERule ::= LHS "::=" Alts ".".
pERule :: Parser ERule
pERule = do
  lhs <- pLHS
  symbol "::="
  alts <- pAlts
  symbol "."
  return (lhs, alts)

-- LHS ::= name OptType | "_".
pLHS :: Parser RLHS
pLHS = 
  do
    n <- pName
    ot <- pOptType
    return (n, RPlain, ot)
  <++
  do
    symbol "_"
    return ("_", RSep, Nothing)

-- OptType ::= | "{:" htext "}".
pOptType :: Parser (Maybe Type)
pOptType = 
  do
    string "{:"
    t <- htext
    symbol "}"
    return (Just (AUser t))
  <++
  return Nothing

-- Alts ::= Seq | Seq "|" Alts.
-- After left factorization:
--  Alts ::= Seq Alts'.
--  Alts' ::= "|" Seq Alts' | ε.
pAlts :: Parser ERHS
pAlts = do
  s <- pSeq
  pAlts' s
  where
    pAlts' :: ERHS -> Parser ERHS
    pAlts' s = do
      symbol "|"
      s' <- pSeq
      as <- pAlts' s'
      return (EBar s as)
      <++
      return s

-- Seq ::= Simple | Simplez "{" htext "}".
pSeq :: Parser ERHS
pSeq = 
  do
    ss <- pSimplez
    string "{"
    t <- htext
    symbol "}"
    return (ESeq ss t)
  <++
    pSimple

-- Simplez ::= | Simple Simplez.
pSimplez :: Parser [ERHS]
pSimplez = pSimplez' []
  where
    pSimplez' :: [ERHS] -> Parser [ERHS]
    pSimplez' ss = do
      s <- pSimple
      pSimplez' (ss ++ [s])
      <++
      return ss

-- Simple ::= Simple0 | Simple0 "?" | Simple0 "*" | "!" Simple0.
-- After left factorization:
--  Simple ::= "!" Simple0 | Simple0 Simple'.
--  Simple' ::= "?" | "*" | ε.
pSimple :: Parser ERHS
pSimple =
  do
    symbol "!"
    s <- pSimple0
    return (ENot s)
  <++
  do
    s <- pSimple0
    pSimple' s
  where
    pSimple' :: ERHS -> Parser ERHS
    pSimple' s = do
      symbol "?"
      return (EOption s)
      <++
      do
        symbol "*"
        return (EMany s)
      <++
      return s

-- Simple0 ::= Atom | Simple0 "{?" htext "}".
-- Eliminated left recursion:
--  Simple0 ::= Atom Simple0'.
--  Simple0' ::= "{?" htext "}" Simple0' | ε.
pSimple0 :: Parser ERHS
pSimple0 =
  do
    a <- pAtom
    pSimple0' a
  where
    pSimple0' :: ERHS -> Parser ERHS
    pSimple0' a = do
      string "{?"
      t <- htext
      symbol "}"
      pSimple0' (EPred a t)
      <++
      return a

-- Atom ::= name | tokLit | "@" | charLit | "(" Alts ")".
pAtom :: Parser ERHS
pAtom = 
  do
    symbol "("
    a <- pAlts
    symbol ")"
    return a
  <++ 
  do
    n <- pName
    return (ESimple (SNTerm n))
  <++
  do
    s <- pTokLit
    return (ESimple (SLit s))
  <++
  do
    symbol "@"
    return (ESimple SAnyChar)
  <++
  do
    c <- pCharLit
    return (ESimple (SChar c))
