-- | Built in functions 
module HogueScript.Functions where

import HogueScript.Expr
import HogueScript.ObjKey
import HogueScript.Eval
import HogueScript.ObjZipper
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
    let setVar =
          case name of
            (Get [identifier]) -> Map.insert (StrKey identifier) value
            _ -> id

    st <- get
    -- shift the zipper right as we need to set the variable in the
    -- super environment
    let envZip = Zipper.right $ Zipper.fromList $ getEnv st
    let envs = maybe
                 (getEnv st)
                 (Zipper.toList . Zipper.set envZip . setVar)
                 (Zipper.get envZip)
    put $ st { getEnv = envs }
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
fnDo ([expr]) = eval expr
fnDo (expr:exprs) = do
    _ <- eval expr
    fnDo exprs
fnDo _ = error "Illegal argument passed to do"

-- Sum function, adds up arguments
{-# ANN fnSum "HLint: ignore Use sum" #-}
fnSum :: [Expr] -> EvalMonad Expr
fnSum exprs = do
    summables <- promoteToSummables exprs
    return $ Lit $ foldr1 (add) summables
  where
    add :: Literal -> Literal -> Literal
    add (B b1) (B b2) = B $ b1 || b2
    add (C _) (C _) = error "Can't add chars"
    add (I i1) (I i2) = I $ i1 + i2
    add (F f1) (F f2) = F $ f1 + f2
    add (S s1) (S s2) = S $ s1 ++ s2
    add _ _ = error "Attempting to add different literal types"

        
promoteToSummables :: [Expr] -> EvalMonad [Literal]
promoteToSummables exprs = do
    literals <- mapM toLiteral exprs
    let commonType = foldr1 getCommonType $ fmap getType literals
    return $ map (promoteLit commonType) literals
  where
    toLiteral :: Expr -> EvalMonad Literal
    toLiteral expr = do
      result <- eval expr
      case result of 
          (Lit l) -> return l
          _ -> error $ show expr ++ " is not equal to literal"

    
    

    
