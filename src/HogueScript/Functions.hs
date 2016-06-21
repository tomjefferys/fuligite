-- | Built in functions 
module HogueScript.Functions where

import HogueScript.Expr
import HogueScript.ObjKey
import HogueScript.Eval
import HogueScript.ObjZipper
import HogueScript.Zipper (Zipper)
import qualified HogueScript.Zipper as Zipper
import HogueScript.Literal

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

defaultEnv :: Object
defaultEnv = Map.fromList [
      (StrKey "var", HFn $ BuiltIn "var" fnVar),
      (StrKey "set", HFn $ BuiltIn "set" fnSet),
      (StrKey "do", HFn $ BuiltIn "do" fnDo),
      (StrKey "+", HFn $ BuiltIn "+" fnSum)]

-- function to declare a variable
-- (var name expr)
fnVar :: [Expr] -> EvalMonad Expr
fnVar [name,value] = do
    st <- get
    let envZip = Zipper.fromList $ getEnv st
    let env' = case name of
            (Get [identifier]) ->
                 Map.insert (StrKey identifier) value (Zipper.get envZip)
            _ -> Zipper.get envZip
    put $ st { getEnv = Zipper.toList $ Zipper.set envZip env' }
    return value
fnVar _ = error "Illegal argument passed to var"

-- function to set a variable
-- (set expr expr)
fnSet :: [Expr] -> EvalMonad Expr
fnSet [name, value] = do
    st <- get
    zipper <- case name of
                (Get path) -> lookupPath $ fmap StrKey path
                _ -> return $ Left $ MSSG "Not a path"
    put $ either (const st) (doSet st value) zipper
    return value
  where
    doSet :: EvalState -> Expr -> ObjZipper -> EvalState
    doSet st val zipper = 
      let zipper' = setZipperExpr zipper val
          (setter, expr) = collapse zipper'
      in case expr of
           (Obj obj) -> setter obj st 
           _ -> error "collapse should return an object"
fnSet _ = error "Illegal argument passed to set"

-- function to execute multiple functions
-- (do expr1 expr2 expr3)
fnDo :: [Expr] -> EvalMonad Expr
fnDo (expr:[]) = eval expr
fnDo (expr:exprs) = do
    _ <- eval expr
    fnDo exprs
fnDo _ = error "Illegal argument passed to do"

-- TODO this should be able to cope with mixed literal, eg Float + Int
-- or String + Bool
-- Sum function, adds up arguments
fnSum :: [Expr] -> EvalMonad Expr
fnSum exprs = do
    nums <- mapM getInt exprs
    let result = foldr (+) 0 nums
    return $ Lit $ I result
  where
    getInt :: Expr -> EvalMonad Int
    getInt n = do
      result <- eval n
      return $ case result of
                  (Lit (I i)) -> i
                  _ -> error "Not an int"
        

    
