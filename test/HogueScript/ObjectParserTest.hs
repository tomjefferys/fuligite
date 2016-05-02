module HogueScript.ObjectParserTest where

import HogueScript.Object
import HogueScript.ObjectParser

import Test.HUnit

import Text.ParserCombinators.Parsec

import Debug.Trace


exprs =
    [("1234", Lit $ I 1234),
     ("\"1234\"", Lit $ S "1234"),
     -- Test accessors
     ("asdf", Get ["asdf"]),
     ("parent.child", Get ["parent","child"]),
     ("parent[\"child\"]", Get ["parent", "child"]),
     ("asdf = 4", Set ["asdf"] $ Lit $ I 4),
     ("{ a : 1.0 b : \"test\"}", Obj $ mkObj % ("a", 1.0 :: Float) % ("b", "test")),
     ("if sdf then wqer = 4 else zxcv = 7",
            If (Get ["sdf"])
            (Set ["wqer"] $ Lit $ I $ 4)
            (Set ["zxcv"] $ Lit $ I $ 7)),
     ("var asdf = \"test\"",
         (Decl ["asdf"] $ Lit $ S $ "test")),
     -- Test Function Application
     ("( asdf )",
         Fapp "asdf" []),
     ("(asdf)",
         Fapp "asdf" []),
     ("(asdf \"qwer\")",
         Fapp "asdf" [Lit $ S $ "qwer"]),
     ("( asdf \"qwer\" )",
         Fapp "asdf" [Lit $ S $ "qwer"]),
     ("(asdf 3 \"qwer\")",
         Fapp "asdf" [Lit $ I $ 3, Lit $ S $ "qwer"]),
     ("( asdf 3 \"qwer\")",
         Fapp "asdf" [Lit $ I $ 3, Lit $ S $ "qwer"]),
     ("(asdf 3 \"qwer\" )",
         Fapp "asdf" [Lit $ I $ 3, Lit $ S $ "qwer"]),
    -- Function declaration
     ("fn a b c -> (sum a b c)",
         Fdec ["a","b","c"] $ Fapp "sum" [(Get ["a"]),(Get ["b"]),(Get ["c"])])]

tests = TestList $ map (\(str,expr) -> TestLabel str $ exprTest str expr) exprs


exprTest str expected = TestCase ( do
             let expr = parse expression "fail" str
             case expr of
               Right e -> assertEqual
                     (str ++ " should parse to " ++ show expected) expected e
               _ -> assertFailure $ "Could not parse: " ++ str) 
