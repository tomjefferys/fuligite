module HogueScript.Variable
( set,
  get) where

import HogueScript.Expr (Expr(..), Variable(..), EvalMonad2,
                          EvalState(..), ObjId, getState)
import qualified Util.IdCache as IdCache

import qualified Control.Monad.State.Strict as State
import qualified HogueScript.Object as Obj
import HogueScript.Path (Path(..))
import Control.Monad.Except
import qualified HogueScript.PropertyList as PropList

get :: Variable -> EvalMonad2 (Maybe Expr)
get (EnvVar eid key) = do
  cache <- envCache <$> State.get
  return $ PropList.lookup key $ getState $ IdCache.getValue eid cache
get (ObjVar oid key) = do
  cache <- objCache <$> State.get
  return $ PropList.lookup key $ IdCache.getValue oid cache
get (ProtoVar proto _ _) = get proto

set :: Variable -> Expr -> EvalMonad2 ()
set (EnvVar eid key) expr = do
  st <- State.get
  let cache     = envCache st
  let env       = IdCache.getValue eid $ envCache st
  let envState' = PropList.insert key expr $ getState env
  let env'      = env { getState                     = envState' }
  let cache'    = IdCache.updateValue eid env' cache
  State.put st { envCache = cache' }

set (ObjVar oid key) expr = do
  --st <- State.get
  obj <- Obj.get oid
  --let cache  = objCache st
  --let obj    = IdCache.getValue oid cache
  let obj'   = PropList.insert key expr obj
  Obj.update oid obj'

-- Set a prot variable, this should set it in the current object
-- creating paths a nescessary
set (ProtoVar _ oid path) expr =
  setPath oid path
  
  where
    -- look up object, create child object if it doesn't exist
    setPath :: ObjId -> Path -> EvalMonad2 ()
    setPath oid' (Item objKey) = 
      Obj.setProp objKey expr oid'
    setPath oid'  (Path objKey path') = do
      mExpr <- Obj.getProp objKey oid'
      case mExpr of
        Just (Obj childId) -> setPath childId path'
        Just _ -> throwError
           "Can't set path on obj, obj contains non obj prop"
        Nothing -> do
          childId <- Obj.new
          Obj.setProp objKey (Obj childId) oid
          setPath childId path'

      
      
