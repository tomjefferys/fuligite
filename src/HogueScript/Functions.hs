-- | Built in functions 
module HogueScript.Functions where

import HogueScript.Expr
import HogueScript.ObjKey
import HogueScript.Eval
import HogueScript.ObjZipper

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

defaultEnv :: Object
defaultEnv = Map.fromList [
      (StrKey "var", HFn $ BuiltIn "var" fnVar),
      (StrKey "set", HFn $ BuiltIn "set" fnSet),
      (StrKey "do", HFn $ BuiltIn "do" fnDo)]

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

-- function to set a variable
-- (set expr expr)
fnSet :: [Expr] -> EvalMonad Expr
fnSet [name, value] = do
    st <- get
    eZipper <- 
          case name of
            -- FIXME lookup path could return an obj path
            (Get path) -> lookupPath $ fmap StrKey path
            _ -> return $ Left $ MSSG "blah"
    let (Obj env') = case eZipper of 
                  Right (ObjZipper zpath _) -> collapse $ ObjZipper zpath value
                  Left _ -> Obj $ getEnv st
    put $ st { getEnv = env' }
    return value
fnSet _ = error "Illegal argument passed to set"

fnDo :: [Expr] -> EvalMonad Expr
fnDo (expr:[]) = eval expr
fnDo (expr:exprs) = do
    _ <- eval expr
    fnDo exprs
fnDo _ = error "Illegal argument passed to do"
    

    
