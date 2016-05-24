-- | Built in functions 
module HogueScript.Functions where

import HogueScript.Expr
import HogueScript.ObjKey

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

defaultEnv :: Object
defaultEnv = Map.fromList [(StrKey "var", HFn $ BuiltIn "var" fnVar)] --Map.empty

-- function to declare a variable
-- (var name expr)
fnVar :: [Expr] -> EvalMonad Expr
fnVar [name,value] = do
    env <- fmap getEnv get
    obj <- fmap getObject get
    fl <- fmap failure get
    let env' = case name of
            (Get [identifier]) -> Map.insert (StrKey identifier) value env
            _ -> env
    put $ EvalState env' obj fl
    return value
fnVar _ = error "Illegal argument passed to var"

