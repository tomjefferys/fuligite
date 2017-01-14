module HogueScript.Environment 
( getParent,
  lookupParent,
  lookupVar,
  getEnv,
  delete,
  getVar,
  setVar,
  pushEnv,
  popEnv,
  pushEnvStack,
  popEnvStack,
  setupEnv,
  empty,
) where

import HogueScript.Expr (Env, EnvId, EvalMonad2, Expr(..),
                          Variable(..), EvalState(..), ObjKey)
import Util.IdCache (IdCache)
import qualified Util.IdCache as IdCache
import HogueScript.Path (Path(..))
import qualified HogueScript.Path as Path
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List.NonEmpty ((<|), NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified HogueScript.Object as Object
import qualified HogueScript.PropertyList as PropList

-- | Gets the parent environment, if
-- it exists
getParent :: EnvId -> EvalMonad2 (Maybe EnvId)
getParent eid = do
  cache <- envCache <$> get
  return $ lookupParent $ IdCache.getValue eid cache

lookupParent :: Env -> Maybe EnvId
lookupParent env =
  case PropList.lookup "__parent" env of 
    (Just (Obj eid)) -> Just eid
    _                -> Nothing

-- | lookups a variable starting in the supllied
-- environment
lookupVar :: Path -> EnvId -> EvalMonad2 (Maybe Variable)
lookupVar path eid = do
  let (key, mPath) = Path.uncons path
  env <- getEnv eid
  let mValue = PropList.lookup key env
  case (mValue, mPath) of
      (Just _,  Nothing)   -> return $ Just $ EnvVar eid key
      (Just (Obj oid), Just path') -> Object.lookupVar path' oid
      _         -> maybe (return Nothing)
                                  (lookupVar path)
                                  (lookupParent env)

getEnv :: EnvId -> EvalMonad2 Env
getEnv eid = IdCache.getValue eid . envCache <$> get

delete :: EnvId -> EvalMonad2 ()
delete eid = do
  st <- get
  let cache = IdCache.removeValue eid $ envCache st
  put st { envCache = cache }


-- | gets a varaible from the name environment
-- throws exception if key is not found
getVar :: EnvId -> ObjKey -> EvalMonad2 Expr
getVar eid key = do
  env <- getEnv eid
  let mVal = PropList.lookup key env
  maybe (throwError notFoundMssg) return mVal
 where
    notFoundMssg = "Variable " ++ show key ++ " not found."


-- | Sets a variable, overwriting any pre existing value
-- TODO this now creates a new variable, shadowing the old one
-- TODO do we need to differentiate declaration from setting?
setVar :: EnvId -> ObjKey -> Expr -> EvalMonad2 Expr
setVar eid key expr = do
  st <- get
  let cache = envCache st
  let env   = IdCache.getValue eid cache
  let env'  = PropList.insert key expr env
  put st { envCache = IdCache.updateValue eid env' cache}
  return expr

-- | Create a new environment, whose parent is the 
-- current environment.  Set the current env to the 
-- new one.
pushEnv :: EvalMonad2 EnvId
pushEnv = do
  st <- get
  let envStack = getEnvId st
  let (eid, cache) =
       IdCache.addValue
          (empty $ Just $ NonEmpty.head envStack)
          $ envCache st        
          
  put st { getEnvId = eid :| NonEmpty.tail envStack,
           envCache = cache }
  return eid

-- | Pops the current environment from the stack
popEnv :: EvalMonad2 ()
popEnv = do
  st <- get
  let envStack = getEnvId st
  mPId <- getParent $ NonEmpty.head envStack
  case mPId of
    Just pid ->
       put st { getEnvId = pid :| NonEmpty.tail envStack}
    Nothing  -> throwError "Can't pop root environment"
  -- TODO gc?

-- | Push a new environment stack onto the environment stack stack
pushEnvStack :: EnvId -> EvalMonad2 ()
pushEnvStack eid = do
  st <- get
  put $ st { getEnvId = eid <| getEnvId st }

-- | Discard the top of the environment stack stack
popEnvStack :: EvalMonad2 ()
popEnvStack = do
  st <- get
  let (_, mEnvStack) = NonEmpty.uncons $ getEnvId st
  case mEnvStack of
    (Just envStack) -> put st { getEnvId = envStack }
    Nothing -> throwError "Tried to pop global env"



-- | setups a new env cache, and root environment
setupEnv :: (EnvId, IdCache Env)
setupEnv = IdCache.addValue
            (empty Nothing)
            $ IdCache.empty "Environments"

empty :: Maybe EnvId -> Env
empty mEid = 
  case mEid of
    Just eid -> PropList.insert "__parent" (Obj eid) PropList.empty
    Nothing  -> PropList.empty
