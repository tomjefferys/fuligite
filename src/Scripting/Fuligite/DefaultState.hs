module Scripting.Fuligite.DefaultState where

import Scripting.Fuligite.Expr (Expr(..), BuiltIn(..), Object, EvalState)
import Scripting.Fuligite.Functions
import Scripting.Fuligite.Function.Arithmetic (fnSum)
import Scripting.Fuligite.Literal (Literal(..))
import qualified Scripting.Fuligite.PropertyList as PropList
import Scripting.Fuligite.State

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
