module HogueScript.Environment 
( getParent,
  lookupVar,
  getVar,
  setVar,
  pushEnv,
) where

import HogueScript.Expr (Env(..), EnvId, EvalMonad2, Expr(..), Variable,
                          EvalState(..))
import HogueScript.ObjKey
import qualified Util.IdCache as IdCache
import HogueScript.Path (Path(..))
import qualified HogueScript.Path as Path

-- | Gets the parent environment, if
-- it exists
getParent :: EnvId -> EvalMonad2 (Maybe EnvId)
getParent eid = do
  cache <- envCache <$> get
  return $ parent $ IdCache.getValue eid cache

-- | lookups a variable starting in the current
-- environment
lookupVar :: Path -> EvalMonad2 (Maybe Variable)
lookupVar Item key = undefined
lookupVar Path key path = undefined


getVar :: EnvId -> ObjKey -> EvalMonad2 Expr
getVar eid key = undefined

setVar :: EnvId -> ObjKey -> Expr -> EvalMonad2 Expr
setVar eid key expr = undefined

pushEnv :: EvalMonad2 EnvId
pushEnv = undefined




