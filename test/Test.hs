{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Test.Tasty
import Test.Tasty.HUnit

import HogueScript.Expr (Expr(..), EvalState)
import HogueScript.Eval (evalString)
import HogueScript.Literal (Literal(..))
import qualified HogueScript.DefaultState as DS

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- | A test component
data TestComp = Execute String | Assert Expr

class TestCompable a where 
  toTestComp :: a -> TestComp

instance TestCompable String where
  toTestComp = Execute 

instance TestCompable Expr where
  toTestComp = Assert 

infixl 5 %
(%) :: (TestCompable a) => [TestComp] -> a -> [TestComp]
lst % comp = lst ++ [toTestComp comp]

testScript :: [TestComp]
testScript = []

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ simpleTest,
    simpleAssignment,
    testObjectProperties,
    testCustomFunction,
    testClosure]

hyaliteTest :: String -> [TestComp] -> TestTree
hyaliteTest title components =
  testCase title $ runTest (Null, DS.new) components []
  where
    runTest :: (Expr, EvalState) -> [TestComp] -> [String] -> Assertion
    runTest _ [] _ = assertBool "All ok" True
    runTest (expr, st) (comp:comps) prog = 
      case comp of 
        (Assert expected) -> do
          assertEqual (getError prog) expected expr
          --expected @=? expr
          runTest (expr, st) comps prog
        (Execute str) -> 
          case evalString st str of
            Left err -> assertBool err False
            Right (expr', st')
               -> runTest (expr', st') comps (str:prog)
    getError :: [String] -> String
    getError = foldr (\expr str -> str ++ " " ++ expr) ""
  

simpleTest =
  hyaliteTest "Test variable declaration" $
    testScript
       % "(var a 5)" % (Lit $ I 5)

simpleAssignment =
  hyaliteTest "Test assignment" $
    testScript
       % "(var a 5)"
       % "(set a 7)"
       % "a" % (Lit $ I 7)

testObjectProperties = 
  hyaliteTest "Test object properties" $
    testScript
    % "(var o {r:1.0 g:0.5 b:0.25})"
    % "o.g" % (Lit $ F 0.5)
    % "(set o.g o.r)"
    % "o.g" % (Lit $ F 1.0)

testCustomFunction = 
  hyaliteTest "Test custom function" $
    testScript
    % "(var sum (fn {a b} (+ a b)))"
    % "(sum 2 3)" % (Lit $ I 5)
    % "(var a 4)"
    % "(var b 6)"
    % "(sum a b)" % (Lit $ I 10)

testClosure = 
  hyaliteTest "Test simple closure" $
    testScript
    % "(var mkAdd (fn {n} (fn {m} (+ m n))))"
    % "(var add2 (mkAdd 2))"
    % "(add2 3)" % (Lit $ I 5)
    % "(add2 10)" % (Lit $ I 12)



    

