module HogueScript.DefaultState where

import HogueScript.Expr (Expr(..), BuiltIn(..), Object, EvalState, makeEvalState)
import HogueScript.Functions
import HogueScript.Literal (Literal(..))
import qualified HogueScript.PropertyList as PropList

defaultEnv :: Object
defaultEnv = PropList.fromList [
      ("stdout", Lit $ S ""),
      ("var", HFn $ BuiltIn "var" fnVar),
      ("set", HFn $ BuiltIn "set" fnSet),
      ("do", HFn $ BuiltIn "do" fnDo),
      ("fn", HFn $ BuiltIn "fn" fnFn),
      ("+", HFn $ BuiltIn "+" fnSum),
      ("print", HFn $ BuiltIn "print" fnPrint)]

new :: EvalState
new = makeEvalState defaultEnv 
