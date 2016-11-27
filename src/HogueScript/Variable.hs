module HogueScript.Variable
( set,
  get) where

import HogueScript.Expr (Expr(..), Variable(..), EvalMonad2,
                          EvalState(..), getState)
import qualified Util.IdCache as IdCache

import qualified Data.Map as Map
import qualified Control.Monad.State.Strict as State
import qualified HogueScript.Object as Obj

get :: Variable -> EvalMonad2 (Maybe Expr)
get (EnvVar eid key) = do
  cache <- envCache <$> State.get
  return $ Map.lookup key $ getState $ IdCache.getValue eid cache
get (ObjVar oid key) = do
  cache <- objCache <$> State.get
  return $ Map.lookup key $ IdCache.getValue oid cache
get (ProtoVar proto _ _) = get proto

set :: Variable -> Expr -> EvalMonad2 ()
set (EnvVar eid key) expr = do
  st <- State.get
  let cache     = envCache st
  let env       = IdCache.getValue eid $ envCache st
  let envState' = Map.insert key expr $ getState env
  let env'      = env { getState                     = envState' }
  let cache'    = IdCache.updateValue eid env' cache
  State.put st { envCache = cache' }

set (ObjVar oid key) expr = do
  --st <- State.get
  obj <- Obj.get oid
  --let cache  = objCache st
  --let obj    = IdCache.getValue oid cache
  let obj'   = Map.insert key expr obj
  Obj.update oid obj'

  --let cache' = IdCache.updateValue oid obj' cache
  --State.put st { objCache = cache' } 
