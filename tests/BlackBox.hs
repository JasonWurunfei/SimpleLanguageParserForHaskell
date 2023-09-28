-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the APpy APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Definitions
import Parser
import Transformer

import Test.Tasty
import Test.Tasty.HUnit
import Data.List

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Smoke tests" [
    ---------- Parser tests ----------
    -- Positive tests
    let str = "---\n E ::= Exp*."
        eg  = [(("E", RPlain, Nothing),
                      EMany (ESimple (SNTerm "Exp")))]
    in testCase "Repeated grammar elements" $
       parseSpec str @?= Right ("", eg),

    let str = "---\n E ::= Exp?."
        eg  = [(("E", RPlain, Nothing),
                      EOption (ESimple (SNTerm "Exp")))]
    in testCase "Optional grammar elements" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n NoWord ::=!Word."
        eg  = [(("NoWord", RPlain, Nothing),
                  ENot (ESimple (SNTerm "Word")))]
    in testCase "Not possible decoration 1" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n Num ::= Digit Digit* !Digit {_1:_2}."
        eg  = [(("Num", RPlain, Nothing),
                  ESeq [ESimple (SNTerm "Digit"),
                        EMany (ESimple (SNTerm "Digit")),
                        ENot (ESimple (SNTerm "Digit"))]
                        "_1:_2")]
    in testCase "Not possible decoration 2" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n Exps{:[Expr]} ::= Exp Exp* {_1 : _2}."
        eg  = [(("Exps", RPlain, Just (AUser "[Expr]")),
                  ESeq [ESimple (SNTerm "Exp"),
                  EMany (ESimple (SNTerm "Exp"))] "_1 : _2")]
    in testCase "User-defined type of a nonterminal's meaning" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n Letter ::= Char{?isAlpha}."
        eg  = [(("Letter", RPlain, Nothing),
                  EPred (ESimple (SNTerm "Char")) "isAlpha")]
    in testCase "Predicate clause" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n AsciiLetter ::= Char{?isAscii}{?isLetter}."
        eg  = [(("AsciiLetter", RPlain, Nothing),
                  EPred (EPred (ESimple (SNTerm "Char")) "isAscii") "isLetter")]
    in testCase "Multiple predicate clauses" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n PlusSign ::= '+'."
        eg  = [(("PlusSign", RPlain, Nothing), ESimple (SChar '+'))]
    in testCase "Single character literal" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n AnyChar ::= @."
        eg  = [(("AnyChar", RPlain, Nothing), ESimple SAnyChar)]
    in testCase "Any character" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n Digit ::= @{?isDigit}."
        eg  = [(("Digit", RPlain, Nothing), EPred (ESimple SAnyChar) "isDigit")]
    in testCase "Any character with predicate" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n T ::= !A{?p}."
        eg  = [(("T", RPlain, Nothing), ENot (EPred (ESimple (SNTerm "A")) "p"))]
    in testCase "Predicate clause groups tigher than not" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n Exp ::= \"let\" id \"=\" Exp \"in\"" ++
              "Exp {Let {{var = _2, def = _4, body = _6}}}."
        eg  = [(("Exp", RPlain, Nothing),
                  ESeq [ESimple (SLit "let"),
                        ESimple (SNTerm "id"),
                        ESimple (SLit "="),
                        ESimple (SNTerm "Exp"),
                        ESimple (SLit "in"),
                        ESimple (SNTerm "Exp")]
                        "Let {var = _2, def = _4, body = _6}")]
    in testCase "Double braces in Action" $
       parseSpec str @?= Right ("", eg),

    let str = "---\n Exp{:{{some haskell code}}} ::= @. "
        eg  = [(("Exp", RPlain, Just (AUser "{some haskell code}")),
                  ESimple SAnyChar)]
    in testCase "Double braces in type definition" $
       parseSpec str @?= Right ("", eg),

    let str = "---\n Exp ::= Exp (\"+\" {Plus} | \"-\" {Minus}) Term {_2 _1 _3}."
        eg  = [(("Exp", RPlain, Nothing),
                  ESeq [ESimple (SNTerm "Exp"),
                        EBar (ESeq [ESimple (SLit "+")] "Plus" )
                             (ESeq [ESimple (SLit "-")] "Minus"),
                        ESimple (SNTerm "Term")]
                        "_2 _1 _3")]
    in testCase "Nested choices" $
        parseSpec str @?= Right ("", eg),

    let str = "  \t \n text with leading whitespace \n---\n AnyChar ::= @."
        eg  = [(("AnyChar", RPlain, Nothing), ESimple SAnyChar)]
        s   = "  \t \n text with leading whitespace \n"
    in testCase "Leading whitespace in preamble" $
        parseSpec str @?= Right (s, eg),

    let str = "---\n AnyChar ::=-- some random comment \n @. "
        eg  = [(("AnyChar", RPlain, Nothing), ESimple SAnyChar)]
    in testCase "Mix comment in the grammar rule 1" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n -- some random comment \n AnyChar -- some random comment \n"++
              "::=-- some random comment \n @-- comment\n. -- comment\n"
        eg  = [(("AnyChar", RPlain, Nothing), ESimple SAnyChar)]
    in testCase "Mix comment in the grammar rule 2" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n AnyChar ::= @ | {()}."
        eg  = [(("AnyChar", RPlain, Nothing),
                  EBar (ESimple SAnyChar) (ESeq [] "()"))]
    in testCase "Absalon alternative" $
        parseSpec str @?= Right ("", eg),

    let str = "---\n S ::= S \"a\" {_1+1} | \"b\" {0}.\n _ ::= {()}."
        eg  = [(("S", RPlain, Nothing),
                EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                          (ESeq [ESimple (SLit "b")] "0")),
               (("_", RSep, Nothing), ESeq [] ("()"))]
    in testCase "Provided parser sample test" $
       parseSpec str @?= Right ("", eg),

    -- Negative tests
    let str = "AnyChar ::= @."
        testName = "No preamble"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let str = "---\n"
        testName = "Empty grammar"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let str = "---\nAnyChar ::= @"
        testName = "No terminating dot"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let str = "---\nAnyChar ::= @ { {} }."
        testName = "Undoubled braces in action 1"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let str = "---\nAnyChar ::= @ { { }."
        testName = "Undoubled braces in action 2"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let str = "---\nAnyChar ::= @ { } }."
        testName = "Undoubled braces in action 3"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let str = "---\nAnyChar{: {} } ::= @."
        testName = "Undoubled braces in user-defined type 1"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let str = "---\nAnyChar{: { } ::= @."
        testName = "Undoubled braces in user-defined type 2"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let str = "---\nAnyChar{: } } ::= @."
        testName = "Undoubled braces in user-defined type 3"
    in testCase testName $
        case parseSpec str of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    ---------- Transformer tests ----------
    -- Positive tests
    let eg = [(("S", RPlain, Nothing),
                EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                     (ESeq [ESimple (SLit "b")] "0")),
              (("_", RSep, Nothing), ESeq [] ("()"))]
        g  = [(("S", RPlain, Nothing),
                [([SNTerm "S", SLit "a"], AUser "_1+1"),
                 ([SLit "b"], AUser "0")]),
                (("_", RSep, Nothing), [([], AUser "()")])]
    in testCase "Provided transformer sample test" $
       convert eg @?= Right g,

    let eg = [(("Exp", RPlain, Nothing),
                ESeq [ESimple (SNTerm "Exp"),
                    EBar (ESeq [ESimple (SLit "+")] "Plus" )
                            (ESeq [ESimple (SLit "-")] "Minus"),
                    ESimple (SNTerm "Term")]
                    "_2 _1 _3"),
                (("_", RSep, Nothing), ESeq [] "()")]
        g  = [(("Exp", RPlain, Nothing),
                [([SNTerm "Exp", SNTerm "Exp'", SNTerm "Term"],
                AUser "_2 _1 _3")]),
              (("Exp'", RPlain, Nothing),
                [([SLit "+"], AUser "Plus"),
                 ([SLit "-"], AUser "Minus")]),
              (("_", RSep, Nothing), [([], AUser "()")])]
        testName = "Choice in alternatives"
    in testCase testName $
        case convert eg of
            Left _ -> assertFailure testName
            -- order is implimentation dependent
            Right g' -> sortGrammar g' @?= sortGrammar g,

    -- let eg = [
    --             (("A", RPlain, Nothing),
    --             ESeq [ESimple (SLit "a"), EOption (ESimple (SNTerm "B"))] "A"),
    --             (("B", RPlain, Nothing), ESimple (SLit "b")),
    --             (("_", RSep, Nothing), ESeq [] "()")
    --         ]
    --     g  = [
    --             (("A", RPlain, Nothing),
    --             [([SLit "a", SNTerm "A'"], AUser "A")]),
    --             (("A'", RPlain, Nothing), 
    --             [([SNTerm "B"], ACst "(Just _1)"), ([], ACst "(Nothing)")]),
    --             (("B", RPlain, Nothing), [([SLit "b"], AUser "()")]),
    --             (("_", RSep, Nothing), [([], AUser "()")])
    --         ]
    --     testName = "Option in alternatives"
    -- in testCase testName $
    --     case convert eg of
    --         Left _ -> assertFailure testName
    --         -- order is implimentation dependent
    --         Right g' -> sortGrammar g' @?= sortGrammar g,

    -- Negative tests
    let eg = [(("S", RPlain, Nothing),
                EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                     (ESeq [ESimple (SLit "b")] "0"))]
        testName = "Missing separator rule"
    in testCase testName $
        case convert eg of
            Left _ -> return ()
            Right _ -> assertFailure testName,

    let eg = [(("S", RPlain, Nothing),
                EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                     (ESeq [ESimple (SLit "b")] "0")),
              (("S", RPlain, Nothing),
                ESeq [ESimple (SLit "c")] "1"),
              (("_", RSep, Nothing), ESeq [] ("()"))]
        testName = "Duplicate non-terminal names"
    in testCase testName $
        case convert eg of
            Left _ -> return ()
            Right _ -> assertFailure testName
  ]

-- helper functions
-- sort a grammar by non-terminal name
sortGrammar :: Grammar -> Grammar
sortGrammar = sortBy (\((nt1,_,_), _) ((nt2,_,_), _) -> compareNT nt1 nt2)

-- compare non-terminals names alphabetically
compareNT :: NTName -> NTName -> Ordering
compareNT [] [] = EQ
compareNT (c1:cs1) [] = GT
compareNT [] (c2:cs2) = LT
compareNT (c1:cs1) (c2:cs2)
  | c1 == c2 = compareNT cs1 cs2
  | c1 < c2 = LT
  | otherwise = GT

