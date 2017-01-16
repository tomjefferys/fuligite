module Scripting.Fuligite.DefaultState where

import Scripting.Fuligite.Expr (Expr(..), BuiltIn(..), Object, EvalState)
import Scripting.Fuligite.Functions
import Scripting.Fuligite.Function.Arithmetic 
import Scripting.Fuligite.Function.Control
import Scripting.Fuligite.Literal (Literal(..))
import qualified Scripting.Fuligite.PropertyList as PropList
import Scripting.Fuligite.State

defaultEnv :: Object
defaultEnv = PropList.fromList [
      ("stdout", Lit $ S ""),
      ("if", HFn $ BuiltIn "if" fnIf),
      ("while", HFn $ BuiltIn "while" fnWhile),
      ("var", HFn $ BuiltIn "var" fnVar),
      ("set", HFn $ BuiltIn "set" fnSet),
      ("do", HFn $ BuiltIn "do" fnDo),
      ("fn", HFn $ BuiltIn "fn" fnFn),
      ("+", HFn $ BuiltIn "+" fnSum),
      ("-", HFn $ BuiltIn "-" fnSubtract),
      ("*", HFn $ BuiltIn "*" fnProduct),
      ("/", HFn $ BuiltIn "/" fnDiv),
      ("%", HFn $ BuiltIn "%" fnMod),
      ("print", HFn $ BuiltIn "print" fnPrint)]

new :: EvalState
new = makeEvalState defaultEnv 
