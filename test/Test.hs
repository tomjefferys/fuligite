import Test.Tasty
import Test.Tasty.HUnit

import HogueScript.Expr (Expr(..), EvalState, emptyEvalState)
import HogueScript.Eval (evalString, evalList)
import HogueScript.Literal (Literal(..))
import qualified HogueScript.DefaultState as DS

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
    [1, 2, 3] `compare` [1,2] @?= GT,
    simpleTest, simpleAssignment]
  
-- | execute some expressions, and compare it to an expected result
hyaliteTest :: String -> Expr -> [String] -> TestTree
hyaliteTest title expected exprs =
  testCase title $
    case evalList DS.new exprs of
      Left err -> assertFailure err
      Right (expr, _) -> expected @=? expr

simpleTest =
  hyaliteTest "Test variable declaration"
    (Lit $ I 5) ["(var a 5)"] 

simpleAssignment =
  hyaliteTest "Test assignment"
    (Lit $ I 7) ["(var a 5)", "(set a 7)", "a"] 


                      
    
    
    

