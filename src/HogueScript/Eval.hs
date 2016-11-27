{-# LANGUAGE FlexibleContexts #-}
module HogueScript.Eval where

import HogueScript.Expr
import HogueScript.Literal
import HogueScript.ObjKey
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified HogueScript.Environment as Env
import HogueScript.Path (Path)
import qualified HogueScript.Path as Path
import qualified HogueScript.Object as Obj
import qualified Data.List.NonEmpty as NonEmpty
import qualified HogueScript.Variable as Var

-- | Evaluate an expression
eval :: Expr -> EvalMonad2 Expr
eval expr = 
    case expr of
        (Get prop) -> do
            mExpr <- lookupPath $ Path.fromList $ fmap StrKey prop
            return $ case mExpr of
                        Just expr' -> expr'
                        _ -> Null
        -- If we encounter an object definition,
        -- turn it into a reference
        (ObjDef obj) -> do
          oid <- Obj.set obj
          eval (Obj oid)
          
        (Obj oid) -> do
          obj <- Obj.get oid
          pushObj oid
          obj' <- mapM eval obj
          oid' <- Obj.set obj'
          popObj
          return $ Obj oid'
        (Fapp path args) -> doFunc path args
        e -> return e


-- | Execute a function
doFunc :: [String]          -- ^ The path to the function
       -> [Expr]            -- ^ The function arguments
       -> EvalMonad2 Expr
doFunc path args = do
    -- lookup name, first in obj, then env
    fun <- lookupPath $ Path.fromList $ fmap StrKey path
    let fn = case fun of
                Just (HFn (BuiltIn _ fn')) -> builtInFunc fn'
                Just (Fn eid params def) -> userFunc eid params def
                _ -> error ("unknown function" ++ show path)
    fn args
    --pushEnv *> fn args <* popEnv

builtInFunc :: ([Expr] -> EvalMonad2 Expr)
            -> ([Expr] -> EvalMonad2 Expr)
builtInFunc fn args = Env.pushEnv *> fn args <* Env.popEnv

-- | Execute a user defined function
userFunc :: EnvId -> [String] -> Expr -> [Expr] -> EvalMonad2 Expr
userFunc eid params expr args = do
    -- bind and evaluate arguments 
    evalArgs <- mapM eval args
    let zipped = zip params evalArgs
    --mapM_ dv zipped
    Env.pushEnvStack eid
         *> Env.pushEnv
           *> mapM_ dv zipped
              *> eval expr <* Env.popEnv
                             <* Env.popEnvStack

  where
    dv :: (String,Expr) -> EvalMonad2 Expr 
    dv (param,arg) = declareVar param arg

-- function to declare a variable
-- (var name expr)
declareVar :: String -> Expr -> EvalMonad2 Expr
declareVar name value = do
  let key = StrKey name
  env <- getEnv
  let envState = getState env
  env' <- 
    case Map.lookup key envState of
      Just _ -> throwError $ "Can't redeclare " ++ name
      Nothing ->
         return $ env { getState = Map.insert key value envState }
  setEnv env'
  return value


lookupPath :: Path -> EvalMonad2 (Maybe Expr)
lookupPath path = do
  eid <- getEnvId <$> get
  mVar <- Env.lookupVar path $ NonEmpty.head eid
  case mVar of
    Just var -> Var.get var
    Nothing -> return Nothing

setPath :: Path -> Expr -> EvalMonad2 ()
setPath path expr = do
  eid <- getEnvId <$> get
  mVar <- Env.lookupVar path $ NonEmpty.head eid
  case mVar of
    Just var -> Var.set var expr
    Nothing -> return ()



findExpr :: [ObjKey] -> Expr -> EvalMonad2 (Maybe Expr)
findExpr [] expr = return $ Just expr
findExpr (name:remainder) expr =
  case expr of
    Obj objId -> do
      obj <- Obj.get objId
      case Map.lookup name obj of
        Just expr' -> findExpr remainder expr'
        Nothing -> return Nothing
    _ -> return Nothing

--setPath :: [ObjKey] -> Expr -> EvalMonad2 ()
--setPath [] _ = throwError "Could not find empty path"
--setPath path expr = do
--  env <- getEnv
--  mEnv <- findEnv path env
--  case mEnv of
--    Just (env, _, path) -> 
--      case path of 
--        [name] ->
--          let env' = setVariable name expr env
--          in undefined
--        _ -> throwError "obj setting not supported yet"
--    Nothing -> throwError "Could not find path"

  
-- deprecated
--lookupEnvs :: Env -> [ObjKey] -> EvalMonad2 (Maybe Expr)
--lookupEnvs env path = do
--  mResult <- lookupEnvPath env path
--  case mResult of
--    Just expr -> return $ Just expr
--    Nothing -> 
--      case parent env of
--        Just eid -> do
--          parentEnv <- getEnvById eid
--          lookupEnvs parentEnv path
--        Nothing -> return Nothing
--  
--lookupEnvPath :: Env -> [ObjKey] -> EvalMonad2 (Maybe Expr)
--lookupEnvPath _ [] = return Nothing
--lookupEnvPath env [key] = do
--    let stateObj = getState env
--    return $ Map.lookup key stateObj
--lookupEnvPath env (key:path) = do
--    let stateObj = getState env
--    let mExpr = Map.lookup key stateObj
--    case mExpr of
--      Just (Obj oid) -> lookupObjPath path oid
--      _ -> return Nothing
  

-- | Lookups up a path on the specified object
--lookupObjPath :: [ObjKey] -> ObjId -> EvalMonad2 (Maybe Expr)
--lookupObjPath [] _ = return Nothing
--lookupObjPath [key] oid = do
--  obj <- getObj $ Obj oid
--  return $ Map.lookup key obj
--lookupObjPath (key:path) oid = do
--  obj <- getObj $ Obj oid
--  let expr = Map.lookup key obj
--  case expr of
--    Just (Obj objId) -> lookupObjPath path objId
--      --getObj (Obj objId) >>= lookupObjPath path
--    _ -> return Nothing
      
      

---- | Lookups up a path
---- Checks in the local object, then in the environment
--lookupPath :: [ObjKey] -- ^ The path
--           -> EvalMonad2 (Either PropError ObjZipper)
--lookupPath path = do
--    obj <- fmap getObject get
--    env <- getEnv
--    let objResult = getPath (ObjZipper setObj [] (Obj obj)) path
--    let envResult = lookupEnvPath env path
--    return $ either (const envResult) Right objResult
--
--lookupEnvPath :: Env
--              -> [ObjKey]
--              -> EvalMonad2 $ Either PropError ObjZipper
--lookupEnvPath env path = do
--  let envObj = getState env
--  
--  let result = getPath (ObjZipper (set 
--
--lookupObjPath
--  
--
---- | Lookups up a path in a list of environments
--lookupEnvPath :: Env -> [ObjKey] -> Either PropError ObjZipper
--lookupEnvPath env path = 
--    let result = Zipper.find findFn (Zipper.fromList envs)
--    in case result of
--        Just zipper -> Right zipper
--        Nothing -> Left $ NO_SUCH_PATH path
--  where 
--    findFn :: Zipper Object -> Maybe ObjZipper
--    findFn zipper = do
--        obj <- Zipper.get zipper
--        let result = getPath (ObjZipper (setEnv zipper) [] (Obj obj)) path
--        either (const Nothing) Just result


-- | Logs a failure in the state
logFailure :: String    -- ^ The failure message
           -> EvalState -- ^ The current state
           -> EvalState -- ^ The updated state
logFailure str evalSt = evalSt { failure = Just str }

-- | Set a property of the local object to an expression.
--setPropM :: [ObjKey] -> Expr -> EvalMonad2 Expr
--setPropM [propName] value = do
--    propMap <- fmap getObject get
--    let oldExpr = Map.findWithDefault Null propName propMap
--    let propMap' = Map.insert propName value propMap
--    modify (setObject propMap')
--    return oldExpr
--setPropM _ _ = error "setPropM must be supplied with a value"
--
---- | sets the local object
--setObject :: Object -> EvalState -> EvalState
--setObject propMap evalSt = evalSt { getObject = propMap }

-- | Constructs an evaluation state
--makeEvalState :: Object     -- ^ The environment
--              -> Object     -- ^ The local object
--              -> EvalState  -- ^ Returns a new EvalState
--makeEvalState env obj = 
--    EvalState IntMap.empty [env] obj Nothing

-- | Evaluates a property coercing its value into a string
evalPropString :: [ObjKey]
               -> Object
               -> Object
               -> (Either String) String
evalPropString path env obj = 
    evalEM (makeEvalState env obj) evalFn
  where
    evalFn :: EvalMonad2 String
    evalFn = getProp path >>= evalToString
    evalToString :: Expr -> EvalMonad2 String
    evalToString (Lit l) = return $ toString l
    evalToString expr = eval expr >>= evalToString


-- | Get the expression associated with a property
getProp :: [ObjKey] -> EvalMonad2 Expr
getProp props = do
    mOid <- fmap getObject get
    --obj <- getObj oid
    
    case mOid of
      Nothing -> throwError "No Object"
      (Just oid) -> getPropFromObj props $ Obj oid

getPropFromObj :: [ObjKey]
                  -> Expr
                  -> EvalMonad2 Expr
getPropFromObj (prop:subprops) (Obj oid) = do
    obj <- Obj.get oid
    let mVal = Map.lookup prop obj
    val <- case mVal of
            Just val -> return val
            Nothing -> throwError $ show $  NO_SUCH_PROP prop
    case subprops of
      [] -> return val
      _ -> getPropFromObj subprops val
getPropFromObj (prop:_) _ =
    throwError $ show $ NO_SUCH_PROP prop
getPropFromObj [] _ =
    throwError $ show $ NO_SUCH_PROP $ StrKey "[]"

-- | Log a failure.  Not an error in the code, but 
-- an expected failure (eg trying to open an open door)
failExpr :: String -> EvalMonad2 Expr
failExpr str = do
    modify $ logFailure str
    return Null
