module HogueScript.DefaultState where

import HogueScript.Expr (Expr(..), BuiltIn(..), Object, EvalState, makeEvalState)
import HogueScript.Functions
import HogueScript.Literal (Literal(..))
import HogueScript.Object (mkObj)
import HogueScript.ObjKey (ObjKey(..))

import qualified Data.Map as Map

defaultEnv :: Object
defaultEnv = Map.fromList [
      (StrKey "stdout", Lit $ S ""),
      (StrKey "var", HFn $ BuiltIn "var" fnVar),
      (StrKey "set", HFn $ BuiltIn "set" fnSet),
      (StrKey "do", HFn $ BuiltIn "do" fnDo),
      (StrKey "fn", HFn $ BuiltIn "fn" fnFn),
      (StrKey "+", HFn $ BuiltIn "+" fnSum),
      (StrKey "print", HFn $ BuiltIn "print" fnPrint)]

new :: EvalState
new = makeEvalState defaultEnv mkObj
