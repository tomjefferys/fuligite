module HogueScript.Function.Arithmetic 
( fnSum,
  fnSubtract,
  fnProduct,
  fnDiv,
  fnMod
) where

import Control.Monad.Except

import HogueScript.Expr (Expr(..), EvalMonad2)
import HogueScript.Eval (eval)
import HogueScript.Literal (Literal(..), getType,
                            getCommonType, promoteLit)

-- Sum function, adds up arguments
{-# ANN fnSum "HLint: ignore Use sum" #-}
fnSum :: [Expr] -> EvalMonad2 Expr
fnSum = doLitFn "sum" add
  where
    add (B b1) (B b2) = return $ B $ b1 || b2
    add (I i1) (I i2) = return $ I $ i1 + i2
    add (F f1) (F f2) = return $ F $ f1 + f2
    add (S s1) (S s2) = return $ S $ s1 ++ s2
    add l1 l2 = arithError "sum" l1 l2

arithError :: String -> Literal -> Literal -> EvalMonad2 Literal
arithError str l1 l2 =
  throwError 
    $ "Can't " ++ str ++ " " ++ show l1 ++ " and " ++ show l2

fnSubtract :: [Expr] -> EvalMonad2 Expr
fnSubtract = doLitFn "subtract" sub
  where
    sub (I i1) (I i2) = return $ I $ i1 + i2
    sub (F f1) (F f2) = return $ F $ f1 + f2
    sub l1 l2 = arithError "subtract" l1 l2

fnProduct :: [Expr] -> EvalMonad2 Expr
fnProduct = doLitFn "product" prod
  where
    prod (I i1) (I i2) = return $ I $ i1 * i2
    prod (F f1) (F f2) = return $ F $ f1 * f2
    prod l1 l2 = arithError "multiply" l1 l2    

fnDiv :: [Expr] -> EvalMonad2 Expr
fnDiv = doLitFn "divide" divide
  where
    divide (I i1) (I i2) = return $ I $ i1 `div` i2
    divide (F f1) (F f2) = return $ F $ f1 / f2
    divide l1 l2 = arithError "divide" l1 l2    

fnMod :: [Expr] -> EvalMonad2 Expr
fnMod = doLitFn "modulo" modulo
  where
    modulo (I i1) (I i2) = return $ I $ i1 `mod` i2
    modulo l1 l2 = arithError "modulo" l1 l2

-- TODO abs negate

doLitFn :: String
        -> (Literal -> Literal -> EvalMonad2 Literal)
        -> [Expr]
        -> EvalMonad2 Expr
doLitFn str fn exprs = do
  literals <- mapM eval exprs
  summables <- promoteToCommonType literals
  result <- 
    case summables of
      (x:xs@(_:_)) -> foldM fn x xs
      _ -> throwError $ "Not enough args passed to " ++ str
  return $ Lit result
  

        
promoteToCommonType :: [Expr] -> EvalMonad2 [Literal]
promoteToCommonType exprs = do
  literals <- mapM toLiteral exprs
  let commonType = foldr1 getCommonType $ fmap getType literals
  return $ map (promoteLit commonType) literals
  where
    toLiteral :: Expr -> EvalMonad2 Literal
    toLiteral expr = do
      result <- eval expr
      case result of 
          (Lit l) -> return l
          _ -> throwError $ show expr ++ " is not equal to literal"
