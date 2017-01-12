{-# LANGUAGE FlexibleInstances #-}
module HogueScript.Object where

import HogueScript.Expr
  (Object, ObjId, EvalMonad2, EvalState(..), Variable(..),
   Expr(..))
import HogueScript.Path (Path(..))
import qualified Util.IdCache as IdCache
import qualified HogueScript.Path as Path
import qualified HogueScript.PropertyList as PropList
import qualified Control.Monad.State.Strict as State
import Control.Monad.Except

mkObj :: Object
mkObj = PropList.empty


-- Gets an object from the state
get :: ObjId -> EvalMonad2 Object 
get oid = do
    cache <- objCache <$> State.get
    case IdCache.lookup oid cache of 
      Just obj -> return obj
      Nothing  -> throwError "not an object"

delete :: ObjId -> EvalMonad2 ()
delete oid = do
  st <- State.get
  let cache = IdCache.removeValue oid $ objCache st
  State.put st { objCache = cache }

update :: ObjId -> Object -> EvalMonad2 ()
update oid obj = do
  st <- State.get
  let cache  = objCache st
  let cache' = IdCache.updateValue oid obj cache
  State.put st { objCache = cache' }

new :: EvalMonad2 ObjId
new = set mkObj

set :: Object -> EvalMonad2 ObjId
set obj = do
  st <- State.get
  let cache = objCache st
  let (oid, cache') = IdCache.addValue obj cache
  State.put st { objCache = cache' }
  return oid

setProp :: String -> Expr -> ObjId -> EvalMonad2 ()
setProp key value oid = do
  obj <- get oid
  update oid $ PropList.insert key value obj

getProp :: String -> ObjId -> EvalMonad2 (Maybe Expr)
getProp key oid = do
  obj <- get oid
  return $ PropList.lookup key obj


-- | Lookup a variable in an object
lookupVar :: Path -> ObjId -> EvalMonad2 (Maybe Variable)
lookupVar path oid = do
  let (key, mPath) = Path.uncons path 
  obj <- get oid
  let mValue = PropList.lookup key obj
  mValue' <-
      case (mValue, mPath) of
        (Just _, Nothing) -> return $ Just $ ObjVar oid key
        (Just (Obj oid'), Just path') -> lookupVar path' oid'
        _ -> return Nothing 
  maybe
   (lookupProtoVar path oid) 
   (return . Just)
   mValue'


-- | Lookup a variable in an objects prototype(s)
lookupProtoVar :: Path -> ObjId -> EvalMonad2 (Maybe Variable)
lookupProtoVar path oid = do
  obj <- get oid
  let mPrototype = PropList.lookup "__protos" obj
  case mPrototype of
    Nothing -> return Nothing
    Just (Obj protosId) -> do
      obj' <- get protosId
      doProtoListLookup $ PropList.elems obj'
    _ -> throwError "__protos is not an object"
  where
    -- | __protos should be a list of prototypes, iterate through 
    --   them looking for a match
    doProtoListLookup :: [Expr] -> EvalMonad2 (Maybe Variable)
    doProtoListLookup [] = return Nothing
    doProtoListLookup (expr:exprs) = do
      mVar <- doProtoLookup expr
      case mVar of
        Nothing -> doProtoListLookup exprs
        result  -> return result
    
    -- | attempt to find a variable within an individual prototype
    doProtoLookup :: Expr -> EvalMonad2 (Maybe Variable)
    doProtoLookup (Obj poid)  = do
      mvar <- lookupVar path poid
      let result =
            case mvar of
              Just var@(ObjVar _ _)   -> Just $ ProtoVar var oid path
              Just (ProtoVar var _ _) -> Just $ ProtoVar var oid path
              _ -> Nothing
      return result
    doProtoLookup _ = throwError "__protos item is not Object"


