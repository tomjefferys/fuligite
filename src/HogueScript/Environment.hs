module HogueScript.Environment 
( getParent,
  lookupVar,
  getVar,
  setVar,
  pushEnv,
  popEnv,
  setupEnv,
) where

import HogueScript.Expr (Env(..), EnvId, EvalMonad2, Expr(..),
                          Variable(..), EvalState(..))
import HogueScript.ObjKey
import Util.IdCache (IdCache)
import qualified Util.IdCache as IdCache
import HogueScript.Path (Path(..))
import qualified HogueScript.Path as Path
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except

-- | Gets the parent environment, if
-- it exists
getParent :: EnvId -> EvalMonad2 (Maybe EnvId)
getParent eid = do
  cache <- envCache <$> get
  return $ parent $ IdCache.getValue eid cache

-- | lookups a variable starting in the supllied
-- environment
lookupVar :: Path -> EnvId -> EvalMonad2 (Maybe Variable)
lookupVar path eid = do
  let (key, mPath) = Path.uncons path
  env <- getEnv eid
  let member = Map.member key $ getState env
  case (member, mPath) of
    (True,  Nothing)   -> return $ Just $ EnvVar eid key
    (True,  Just _)    -> undefined -- TODO object lookup
    (False, _)         -> maybe (return Nothing)
                                (lookupVar path)
                                (parent env)

getEnv :: EnvId -> EvalMonad2 Env
getEnv eid = IdCache.getValue eid . envCache <$> get

-- | gets a varaible from the name environment
-- throws exception if key is not found
getVar :: EnvId -> ObjKey -> EvalMonad2 Expr
getVar eid key = do
  env <- getEnv eid
  let mVal = Map.lookup key $ getState env
  maybe (throwError notFoundMssg) return mVal
 where
    notFoundMssg = "Variable " ++ show key ++ " not found."


-- | Sets a variable, overwriting any pre existing value
-- TODO do we need to differentiate declaration from setting?
setVar :: EnvId -> ObjKey -> Expr -> EvalMonad2 Expr
setVar eid key expr = do
  st <- get
  let cache = envCache st
  let env   = IdCache.getValue eid cache
  let env'  = env { getState = Map.insert key expr (getState env)}
  put st { envCache = IdCache.updateValue eid env' cache}
  return expr

-- | Create a new environment, whose parent is the 
-- current environment.  Set the current env to the 
-- new one.
pushEnv :: EvalMonad2 EnvId
pushEnv = do
  st <- get
  let (eid, cache) =
       IdCache.addValue
          (makeEnv $ Just $ getEnvId st)
          $ envCache st        
  put st { getEnvId = eid, envCache = cache }
  return eid

-- | Pops the current environment from the stack
popEnv :: EvalMonad2 ()
popEnv = do
  st <- get
  let eid = getEnvId st
  mPId <- getParent eid
  case mPId of
    Just pid -> put st { getEnvId = pid }
    Nothing  -> throwError "Can't pop root environment"
  -- TODO gc?


-- | setups a new env cache, and root environment
setupEnv :: (EnvId, IdCache Env)
setupEnv = IdCache.addValue
            (makeEnv Nothing)
            $ IdCache.empty "Environments"

makeEnv :: Maybe EnvId -> Env
makeEnv mParent = Env mParent Map.empty

