-- | Built in functions 
module HogueScript.Functions where

import HogueScript.Expr (Expr(..), Object, BuiltIn(..), EvalMonad2, getEnv,
                          PropError(..), EvalState, getObj, getIdentifier,
                          parent, getEnvById, setEnvById, setVariable,
                          setObj)
import HogueScript.ObjKey (ObjKey(..))
import HogueScript.Eval (lookupPath, eval)
import HogueScript.ObjZipper (ObjZipper, setZipperExpr, collapse)
import qualified HogueScript.Zipper as Zipper
import HogueScript.Literal (
               Literal(..), toString, getCommonType, getType, promoteLit)

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except

defaultEnv :: Object
defaultEnv = Map.fromList [
      (StrKey "stdout", Lit $ S ""),
      (StrKey "var", HFn $ BuiltIn "var" fnVar),
      --(StrKey "set", HFn $ BuiltIn "set" fnSet),
      (StrKey "do", HFn $ BuiltIn "do" fnDo),
      (StrKey "fn", HFn $ BuiltIn "fn" fnFn),
      (StrKey "+", HFn $ BuiltIn "+" fnSum),
      (StrKey "print", HFn $ BuiltIn "print" fnPrint)]


-- function to declare a variable
-- (var name expr)
fnVar :: [Expr] -> EvalMonad2 Expr
fnVar [name,value] = do
    value' <- eval value
    --let setVar =
    --      case name of
    --        (Get [identifier]) -> Map.insert (StrKey identifier) value'
    --        _ -> id
    let ident =
          case name of
            (Get [i]) -> i
            _ -> throwError "Bad name passed to var"
              
    mEId <- parent <$> getEnv
    eid <- case mEId of
            (Just e) -> return e
            _ -> throwError "Can't get environment"
    env <- getEnvById eid

    let env' = setVariable (StrKey ident) value env
    setEnvById eid env'

    return value'

          
    
    

    --st <- get
    ---- shift the zipper right as we need to set the variable in the
    ---- super environment
    --let envZip = Zipper.right $ Zipper.fromList $ getEnv st
    --let envs = maybe
    --             (getEnv st)
    --             (Zipper.toList . Zipper.set envZip . setVar)
    --             (Zipper.get envZip)
    --put $ st { getEnv = envs }
    --return value'

fnVar _ = error "Illegal argument passed to var"

-- function to set a variable
-- (set expr expr)
-- Deprectated, doesn't work properly
-- Should probably just return a new object
fnSet :: [Expr] -> EvalMonad2 Expr
fnSet [name, value] = do
    st <- get
    zipper <- case name of
                (Get path) -> lookupPath $ fmap StrKey path
                _ -> throwError "Not a path"
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

-- function definition
fnFn :: [Expr] -> EvalMonad2 Expr
fnFn [params, def] = do
    obj <- getObj params
    let elems = Map.elems obj
    ids <-  mapM getIdentifier elems
    let ids' = map (foldr1 (++)) ids
    def' <- bindVariables elems def
    return $ Fn ids' def'
fnFn args = throwError $ show $ BAD_ARGS args

bindVariables :: [Expr] -> Expr -> EvalMonad2 Expr
bindVariables params expr =
  case expr of
    g@(Get _) -> if g `elem` params
                  then return g
                  else eval g
    (Fapp path exprs) -> 
      Fapp path <$> mapM (bindVariables params) exprs
    o@(Obj _) -> do
      obj <- getObj o     
      obj' <- mapM (bindVariables params) obj
      objId <- setObj obj'
      return $ Obj objId
     -- Obj <$>     _ -> return expr
      

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
    add (C _) (C _) = error "Can't add chars"
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

    
    

    
