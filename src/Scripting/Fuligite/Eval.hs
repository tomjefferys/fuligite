{-# LANGUAGE FlexibleContexts #-}
module Scripting.Fuligite.Eval where

import Scripting.Fuligite.Expr
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Scripting.Fuligite.Environment as Env
import Scripting.Fuligite.Path (Path(..))
import qualified Scripting.Fuligite.Path as Path
import qualified Scripting.Fuligite.Object as Obj
import qualified Scripting.Fuligite.PropertyList as PropList
import qualified Scripting.Fuligite.Literal as Lit
import qualified Data.List.NonEmpty as NonEmpty
import qualified Scripting.Fuligite.Variable as Var
import Scripting.Fuligite.ObjectParser (expression)
import Text.ParserCombinators.Parsec (parse)
import Scripting.Fuligite.GarbageCollector


-- | Evaluate an expression
eval :: Expr -> EvalMonad2 Expr
eval expr = 
    case expr of
        (Get prop) -> do
            mExpr <- lookupPath $ Path.fromList prop
            return $ case mExpr of
                        Just expr' -> expr'
                        _ -> Null
        -- If we encounter an object definition,
        -- turn it into a reference
        (ObjDef obj) -> do
          oid <- Obj.set obj
          eval (Obj oid)
          
        (Obj oid) -> do
          obj  <- Obj.get oid
          obj' <- mapM eval obj
          oid' <- Obj.set obj'
          return $ Obj oid'
        (Fapp path args) -> doFunc path args
        e -> return e

type Result = Either String (Expr, EvalState)

parseExpr :: String -> Either String Expr
parseExpr str = 
  case parse expression "Hyalite error" str of
    Left err   -> Left $ show err
    Right expr -> Right expr

evalString :: EvalState -> String -> Result
evalString st str = do
  expr         <- parseExpr str
  (expr', st') <- evaluate st expr
  (_, st'')    <- doEM st' runGC
  return (expr', st'')
  

evalList :: EvalState -> [String] -> Result
evalList st = 
  foldM (\(_, st') str -> evalString st' str) (Null, st) 

evaluate :: EvalState -> Expr -> Result
evaluate st expr = doEM st (eval expr) 

-- | Execute a function
doFunc :: [String]          -- ^ The path to the function
       -> [Expr]            -- ^ The function arguments
       -> EvalMonad2 Expr
doFunc path args = do
    -- lookup name, first in obj, then env
    let path' = Path.fromList path
    fun   <- lookupPath path'
    mSelf <- case getSelf path' of
                Just path'' -> lookupPath path''
                Nothing -> return Nothing
    let fn = case fun of
                Just (HFn (BuiltIn _ fn')) -> builtInFunc fn'
                Just (Fn eid params def)   -> userFunc eid params def
                _ -> error ("unknown function" ++ show path)
    
    fn args mSelf
    

getSelf :: Path -> Maybe Path
getSelf (Item _) = Nothing
getSelf (Path objKey (Item _)) = Just $ Item objKey
getSelf (Path objKey path) = Path objKey <$> getSelf path

-- | Interpret a boolean value as an expression
boolValue :: Expr -> Bool
boolValue (Lit l) = Lit.boolValue l
boolValue Null    = False
boolValue _       = True


builtInFunc :: ([Expr] -> EvalMonad2 Expr)
            -> ([Expr] -> Maybe Expr ->  EvalMonad2 Expr)
builtInFunc fn args _ = Env.pushEnv *> fn args <* Env.popEnv

-- | Execute a user defined function
userFunc :: EnvId
         -> [String]
         -> Expr
         -> [Expr]
         -> Maybe Expr
         -> EvalMonad2 Expr
userFunc eid params expr args mSelf = do
    -- bind and evaluate arguments 
    evalArgs <- mapM eval args
    let zipped = zip params evalArgs
    Env.pushEnvStack eid
         *> Env.pushEnv
           *> mapM_ dv zipped -- ^ Bind arguments
              *> bindSelf mSelf -- ^ Bind self
                *> eval expr <* Env.popEnv
                               <* Env.popEnvStack

  where
    dv :: (String,Expr) -> EvalMonad2 Expr 
    dv (param,arg) = declareVar param arg
    
    bindSelf :: Maybe Expr -> EvalMonad2 Expr
    bindSelf mExpr = 
      case mExpr of
        Just expr' -> declareVar "self" expr'
        Nothing    -> return Null

-- function to declare a variable
-- (var name expr)
declareVar :: String -> Expr -> EvalMonad2 Expr
declareVar name value = do
  env <- getEnv
  env' <- 
    case PropList.lookup name env of
      Just _  -> throwError $ "Can't redeclare " ++ name
      Nothing -> return $ PropList.insert name value env
  setEnv env'
  return value


lookupPath :: Path -> EvalMonad2 (Maybe Expr)
lookupPath path = do
  eid  <- getEnvId <$> get
  mVar <- Env.lookupVar path $ NonEmpty.head eid
  case mVar of
    Just var -> Var.get var
    Nothing  -> return Nothing

setPath :: Path -> Expr -> EvalMonad2 ()
setPath path expr = do
  eid  <- getEnvId <$> get
  mVar <- Env.lookupVar path $ NonEmpty.head eid
  case mVar of
    Just var -> Var.set var expr
    Nothing  -> return ()

findExpr :: [ObjKey] -> Expr -> EvalMonad2 (Maybe Expr)
findExpr [] expr = return $ Just expr
findExpr (name:remainder) expr =
  case expr of
    Obj objId -> do
      obj <- Obj.get objId
      case PropList.lookup name obj of
        Just expr' -> findExpr remainder expr'
        Nothing    -> return Nothing
    _ -> return Nothing

getPropFromObj :: [ObjKey]
               -> Expr
               -> EvalMonad2 Expr
getPropFromObj (prop:subprops) (Obj oid) = do
    obj <- Obj.get oid
    let mVal = PropList.lookup prop obj
    val <- case mVal of
            Just val -> return val
            Nothing  -> throwError $ show $  NO_SUCH_PROP prop
    case subprops of
      [] -> return val
      _  -> getPropFromObj subprops val
getPropFromObj (prop:_) _ =
    throwError $ show $ NO_SUCH_PROP prop
getPropFromObj [] _ =
    throwError $ show $ NO_SUCH_PROP "[]"
