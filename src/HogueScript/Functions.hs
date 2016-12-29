-- | Built in functions 
module HogueScript.Functions where

import HogueScript.Expr (Expr(..), EvalMonad2, getEnv,
                          parent, PropError(..), getIdentifier,
                          getEnvById, setEnvById, declareVariable,
                          getEnvId)
import HogueScript.Eval (eval)
import HogueScript.Literal (
               Literal(..), toString, getCommonType, getType, promoteLit)

import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Extra (whenJust)
import qualified HogueScript.Path as Path
import qualified HogueScript.Environment as Env
import qualified HogueScript.Object as Obj
import qualified HogueScript.Variable as Var
import qualified Data.List.NonEmpty as NonEmpty
import qualified HogueScript.PropertyList as PropList

-- function to declare a variable
-- (var name expr)
fnVar :: [Expr] -> EvalMonad2 Expr
fnVar [name,value] = do
    value' <- eval value
    ident <-
          case name of
            (Get [i]) -> return i
            _ -> throwError "Bad name passed to var"
              
    mEId <- parent <$> getEnv
    eid <- case mEId of
            (Just e) -> return e
            _ -> throwError "Can't get environment"
    env <- getEnvById eid

    let env' = declareVariable ident value' env
    setEnvById eid env'

    return value'
fnVar args = error ("Illegal argument passed to var: " ++ show args)

-- function to set a variable
-- (set expr expr)
-- Deprectated, doesn't work properly
-- Should probably just return a new object
fnSet :: [Expr] -> EvalMonad2 Expr
fnSet [name, value] = do
    value' <- eval value
    path <- case name of
                (Get path) -> return $ Path.fromList path
                _ -> throwError $ show name ++ " is not a path"
    eid <- getEnvId <$> get
    mVar <- Env.lookupVar path $ NonEmpty.head eid
    whenJust mVar (`Var.set` value')
    return value'
fnSet _ = error "Illegal argument passed to set"

-- function definition
fnFn :: [Expr] -> EvalMonad2 Expr
fnFn [params, def] = do
    obj <- case params of
              Obj oid -> Obj.get oid
              ObjDef o -> return o
              _ -> throwError "Function params is bad type"

    let elems = PropList.elems obj
    ids <-  mapM getIdentifier elems
    let ids' = map (foldr1 (++)) ids
    env <- NonEmpty.head . getEnvId <$> get
    return $ Fn env ids' def
fnFn args = throwError $ show $ BAD_ARGS args

-- appends the expressions to the variable __stdout
fnPrint :: [Expr] -> EvalMonad2 Expr
fnPrint exprs = do
    buffer <- eval (Get ["stdout"])
    stdout <- case buffer of
                Null -> return ""
                (Lit (S val)) -> return val
                _ -> throwError $ show $ INVALID_EXPR buffer "stdout is not string" 
    str <- foldM concatExpr "" exprs
    let stdout' = stdout ++ str
    _ <- fnSet [Get ["stdout"], Lit $ S stdout']
    return $ Lit $ S stdout'
  where
    concatExpr :: String -> Expr -> EvalMonad2 String
    concatExpr acc expr = do
       result <- eval expr
       case result of
                (Lit lit) -> return $ acc ++ toString lit ++ "\n"
                _ -> throwError $ show $
                   INVALID_EXPR result "expression is not printable"
    

-- function to execute multiple functions
-- (do expr1 expr2 expr3)
fnDo :: [Expr] -> EvalMonad2 Expr
fnDo [expr] = eval expr
fnDo (expr:exprs) = do
    _ <- eval expr
    fnDo exprs
fnDo _ = error "Illegal argument passed to do"

-- Sum function, adds up arguments
{-# ANN fnSum "HLint: ignore Use sum" #-}
fnSum :: [Expr] -> EvalMonad2 Expr
fnSum exprs = do
    literals <- mapM eval exprs
    summables <- promoteToSummables literals
    return $ Lit $ foldr1 add summables
  where
    add :: Literal -> Literal -> Literal
    add (B b1) (B b2) = B $ b1 || b2
    add (C _) (C _)   = error "Can't add chars"
    add (I i1) (I i2) = I $ i1 + i2
    add (F f1) (F f2) = F $ f1 + f2
    add (S s1) (S s2) = S $ s1 ++ s2
    add _ _ = error "Attempting to add different literal types"

        
promoteToSummables :: [Expr] -> EvalMonad2 [Literal]
promoteToSummables exprs = do
    literals <- mapM toLiteral exprs
    let commonType = foldr1 getCommonType $ fmap getType literals
    return $ map (promoteLit commonType) literals
  where
    toLiteral :: Expr -> EvalMonad2 Literal
    toLiteral expr = do
      result <- eval expr
      case result of 
          (Lit l) -> return l
          _ -> error $ show expr ++ " is not equal to literal"

    
    

    
