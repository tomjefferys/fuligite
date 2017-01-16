module Scripting.Fuligite.Function.Control 
( fnIf,
  fnWhile
) where

import Control.Monad.Except

import Scripting.Fuligite.Expr (Expr(..), EvalMonad2)
import Scripting.Fuligite.Eval (eval, boolValue)
import Scripting.Fuligite.Literal (Literal(..))

fnIf :: [Expr] -> EvalMonad2 Expr
fnIf [cond, ifTrue, ifFalse] = do
  expr <- eval cond
  if boolValue expr
    then eval ifTrue
    else eval ifFalse
fnIf args = throwError $ "Wrong args passed to if :" ++ show args

fnWhile :: [Expr] -> EvalMonad2 Expr
fnWhile [cond,whileTrue] = doWhile False
  where
    doWhile doneOnce = do
      expr <- eval cond
      if boolValue expr
        then eval whileTrue >> doWhile True
        else return $ Lit $ B doneOnce
fnWhile args = throwError $ "Wrong args passed to while : " ++ show args

