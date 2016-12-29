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
    testIdentifiers,
    simpleAssignment,
    testObjectProperties,
    testCustomFunction,
    testNoParamsFunction,
    testClosure,
    testMultipleInheritence,
    testPrototypeLookup,
    testSelfReference,
    testSelfReference2]

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

testIdentifiers =
  hyaliteTest "Test identifiers" $
    testScript
      % "(var a234 \"one\")"
      % "(var ___ \"two\")"
      % "(var a+b \"three\")"
      % "a234" % (Lit $ S "one")
      % "___" % (Lit $ S "two")
      % "a+b" % (Lit $ S "three")

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

testNoParamsFunction = 
  hyaliteTest "Test no params function" $
    testScript
    % "(var ten (fn {} 10))"
    % "(ten)" % (Lit $ I 10)
    % "(var mkObj (fn {} {a:20 b:30}))"
    % "(var obj1 (mkObj))"
    % "obj1.a" % (Lit $ I 20)
    % "(var obj2 (mkObj))"
    % "obj2.a" % (Lit $ I 20)
    % "(set obj2.a 40)"
    % "obj2.a" % (Lit $ I 40)
    % "obj1.a" % (Lit $ I 20)

testClosure = 
  hyaliteTest "Test simple closure" $
    testScript
    % "(var mkAdd (fn {n} (fn {m} (+ m n))))"
    % "(var add2 (mkAdd 2))"
    % "(add2 3)" % (Lit $ I 5)
    % "(add2 10)" % (Lit $ I 12)

testPrototypeLookup =
  hyaliteTest "Test object prototype lookup" $
    testScript
    % "(var o1 {a:5 b:6})"
    % "(var o2 {__protos:{o1} b:10})"
    % "o2.a" % (Lit $ I 5)
    % "o2.b" % (Lit $ I 10)
    % "(set o2.a 20)"
    % "o2.a" % (Lit $ I 20)
    % "o1.a" % (Lit $ I 5)

testMultipleInheritence = 
  hyaliteTest "Test object with multiple prototypes" $
    testScript
    % "(var o1 {a:5 b:6})"
    % "(var o2 {a:7 b:3})"
    % "(var o3 {__protos: {o2 o1} b:1})"
    % "o3.a" % (Lit $ I 5)
    % "o3.b" % (Lit $ I 1)
    % "(var o4 {__protos: {o1 o2}})"
    % "o4.a" % (Lit $ I 7)
    % "o4.b" % (Lit $ I 3)
    % "(var o5 {__protos:{o2 o3}})"
    % "o5.a" % (Lit $ I 5)

testSelfReference = 
  hyaliteTest "Test implicit self reference" $
    testScript
    % "(var account \
      \  { balance:0 \
      \    deposit: \
      \     (fn {n} (set self.balance (+ self.balance n))) } )"
    % "account.balance" % (Lit $ I 0)
    % "(account.deposit 10)"
    % "account.balance" % (Lit $ I 10)

testSelfReference2 =
  hyaliteTest "Test deeply nested self reference" $
    testScript
    % "(var mkAccount \
      \  (fn {} \
      \    { balance: 0 \
      \      deposit: \
      \        (fn {n} (set self.balance (+ self.balance n)))}))"
    % "(var mkObj (fn {} {balance:3 account:(mkAccount)}))"
    % "(var obj1 (mkObj))"
    % "(var obj2 (mkObj))"
    % "(obj1.account.deposit 123)"
    % "obj1.balance" % (Lit $ I 3)
    % "obj2.account.balance" % (Lit $ I 0)
    % "obj1.account.balance" % (Lit $ I 123)
    % "(obj1.account.deposit 234)"
    % "obj1.account.balance" % (Lit $ I 357)


