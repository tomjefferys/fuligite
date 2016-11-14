{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module HogueScript.Expr where

import HogueScript.Literal
import HogueScript.ObjKey
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Identity
import Util.IdCache (IdCache)
import qualified Util.IdCache as IdCache 

-- The map of properties for an entity
-- TODO if this was just an array of values, with optional 
-- keys, we wouldn't need the differet types of ObjKey
type Object = Map ObjKey Expr

emptyObj :: Object
emptyObj = Map.empty

-- Should object be part of the environment?
-- Should it be passed through as an implicit self reference tp 
-- the function call
type ObjId = Int

-- Represents components of an expression
data Expr = Lit Literal |
            ObjDef Object |
            Obj ObjId |
            Get [String] |
            Fapp [String] [Expr] | -- ^ Function application
            Fn [String] Expr |   -- ^ Function
            HFn BuiltIn | -- ^ A builtin function
            Null 
            deriving (Ord, Eq, Show)

-- Gets an object from the state
getObj :: Expr -> EvalMonad2 Object 
getObj expr = do
    oid <- case expr of
            Obj oid -> return oid
            _ -> throwError $ show $ BAD_TYPE OBJECT
    cache <- objCache <$> get
    let mObj = IdCache.lookup oid cache
    case mObj of 
      Just obj -> return obj
      Nothing  -> throwError "not an object"

setObj :: Object -> EvalMonad2 ObjId
setObj obj = do
  st <- get
  let cache = objCache st
  let (oid, cache') = IdCache.addValue obj cache
  put st { objCache = cache' }
  return oid


getIdentifier :: (MonadError String m)
                  => Expr
                  -> m [String]
getIdentifier expr =
    case expr of
      Get path -> return path
      _ -> throwError $ show $ MSSG $ "Not Get: " ++ show expr


-- Type for a builtin function
-- Takes a name and the function itself
data BuiltIn = BuiltIn String ([Expr] -> EvalMonad2 Expr)

instance Ord BuiltIn where
    compare (BuiltIn name1 _) (BuiltIn name2 _) = 
        compare name1 name2

instance Eq BuiltIn where
    (BuiltIn name1 _) == (BuiltIn name2 _) = name1 == name2 

instance Show BuiltIn where
    show (BuiltIn name _) = name

-- | Variable type, consists of variables owner,
-- and name
data Variable = EnvVar EnvId ObjKey | ObjVar ObjId ObjKey

getVar :: Variable -> EvalMonad2 (Maybe Expr)
getVar (EnvVar eid key) = do
  cache <- envCache <$> get
  return $ Map.lookup key $ getState $ IdCache.getValue eid cache
getVar (ObjVar oid key) = do
  cache <- objCache <$> get
  return $ Map.lookup key $ IdCache.getValue oid cache

setVar :: Variable -> Expr -> EvalMonad2 ()
setVar (EnvVar eid key) expr = do
  st <- get
  let cache     = envCache st
  let env       = IdCache.getValue eid $ envCache st
  let envState' = Map.insert key expr $ getState env
  let env'      = env { getState                     = envState' }
  let cache'    = IdCache.updateValue eid env' cache
  put st { envCache = cache' }

setVar (ObjVar oid key) expr = do
  st <- get
  let cache  = objCache st
  let obj    = IdCache.getValue oid cache
  let obj'   = Map.insert key expr obj
  let cache' = IdCache.updateValue oid obj' cache
  put st { objCache = cache' } 

      
    
  

-- The state of evaluation of a property expression
data EvalState =
    EvalState {
        envCache  :: IdCache Env,
        getEnvId  :: EnvId,
        objCache  :: IdCache Object,
        getObject :: Maybe ObjId,
        failure   :: Maybe String }

emptyEvalState :: EvalState
emptyEvalState = 
  let (eid, cache) = IdCache.addValue (emptyEnv Nothing)
                     $ IdCache.empty "EnvCache"
  in EvalState cache eid (IdCache.empty "ObjCache") Nothing Nothing
  
-- | Constructs an evaluation state
makeEvalState :: Object     -- ^ The environment
              -> Object     -- ^ The local object
              -> EvalState  -- ^ Returns a new EvalState
makeEvalState env obj =
  let (eid, envs) = IdCache.addValue (Env Nothing env)
                        $ IdCache.empty "EnvCache"
      (oid, objs) = IdCache.addValue obj
                        $ IdCache.empty "ObjCache"
  in EvalState envs eid objs (Just oid) Nothing

type EnvId = Int

-- | An environment, the gloval env
-- will not have a parentt
-- TODO could just be an object with a __PARENT?
-- We wouldn't be able to support type safety
data Env = Env {
  parent :: Maybe EnvId,
  getState  :: Object
}

getParent :: Env -> EvalMonad2 (Maybe Env)
getParent env = do
  let mpid = parent env
  case mpid of
    Just pid -> Just <$> getEnvById pid
    Nothing -> return Nothing



setVariable :: ObjKey -> Expr -> Env -> Env
setVariable name value env = 
  let state' = Map.insert name value
                $ getState env
  in env { getState = state' }

-- Envs could be considered like an object
-- Objects parents are not writable though

-- | Make a new empty environment
emptyEnv :: Maybe EnvId -> Env
emptyEnv eid = Env eid emptyObj

getEnvById :: EnvId -> EvalMonad2 Env
getEnvById eid = do
  cache <- envCache <$> get
  return $ IdCache.getValue eid cache

setEnvById :: EnvId -> Env -> EvalMonad2 ()
setEnvById eid env = do
  st <- get
  let cache = envCache st
  let cache' = IdCache.updateValue eid env cache
  put st { envCache = cache' }


getEnv :: EvalMonad2 Env
getEnv = do
  eid <- getEnvId <$> get
  cache <- envCache <$> get
  return $ IdCache.getValue eid cache

setEnv :: Env -> EvalMonad2 ()
setEnv env = do
  st <- get
  let eid = getEnvId st
  let cache = envCache st
  let cache' = IdCache.updateValue eid env cache
  put $ st { envCache = cache' }

-- | push a new environment 
pushEnv :: EvalMonad2 ()
pushEnv = do
    st <- get
    let (eid', envs') = IdCache.addValue
         (emptyEnv $ Just $ getEnvId st)
         $ envCache st
    put st { getEnvId = eid', envCache = envs'}

-- pop and discard the top environment
popEnv :: EvalMonad2 ()
popEnv = do
    st <- get
    let env = IdCache.getValue (getEnvId st) (envCache st)
    put $ maybe st (\p -> st { getEnvId = p }) $ parent env

pushObj :: (MonadState EvalState m) => ObjId -> m ()
pushObj oid = do
    st <- get
    put st { getObject = Just oid }

popObj :: EvalMonad2 ()
popObj = do
    st <- get
    put st { getObject = Nothing } 

-- The Monad stack used when evaluating expressions
newtype EvalMonad2 a = EvalMonad2 {
    runEM :: StateT EvalState (ExceptT String Identity) a
} deriving (Functor,
            Applicative,
            Monad,
            MonadState EvalState,
            MonadError String)

evalEM :: EvalState -> EvalMonad2 a -> Either String a
evalEM st fn = runIdentity
               $ runExceptT
               $ evalStateT (runEM fn) st

doEM :: EvalState -> EvalMonad2 a -> Either String (a, EvalState)
doEM st fn = runIdentity
              $ runExceptT
              $ runStateT (runEM fn) st



data Type = BOOL | CHAR | STRING | INT | FLOAT | OBJECT
            deriving (Show, Eq)
data PropError = BAD_TYPE Type --Type
                 | NO_SUCH_PROP ObjKey
                 | NO_SUCH_PATH [ObjKey]
                 | TRACE Expr PropError
                 | MSSG String
                 | INVALID_EXPR Expr String
                 | BAD_ARGS [Expr]
                 deriving (Show, Eq)

