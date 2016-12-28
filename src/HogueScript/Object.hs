{-# LANGUAGE FlexibleInstances #-}
module HogueScript.Object where

import qualified Data.Map.Strict as Map
import HogueScript.ObjKey
import HogueScript.Expr
  (Object, ObjId, EvalMonad2, EvalState(..), Variable(..),
   Expr(..))
import HogueScript.Path (Path(..))
import qualified Util.IdCache as IdCache
import qualified HogueScript.Path as Path
import qualified Control.Monad.State.Strict as State
import Control.Monad.Except

class ObjKeySrc a where
    getKey :: a -> ObjKey

instance ObjKeySrc Integer where
    getKey = NumKey 

instance ObjKeySrc String where
    getKey = StrKey

--setProp :: (LiteralType a, ObjKeySrc s) => (s, a) -> Object -> Object
--setProp (prop,value) = 
--    Map.insert (getKey prop) (getExpr value)

mkObj :: Object
mkObj = Map.empty

-- TODO move this and setProp to a utilities module
-- | Operator for easy construction of objects, acts as an
-- infix setProp, allows for the following
--  mkObj % ("prop1", "value1") % ("prop2", 11 :: Int)
--infixl 5 %
--(%) :: (LiteralType a, ObjKeySrc s) => Object -> (s, a) -> Object
--obj % property = setProp property obj

-- Gets an object from the state
get :: ObjId -> EvalMonad2 Object 
get oid = do
    cache <- objCache <$> State.get
    case IdCache.lookup oid cache of 
      Just obj -> return obj
      Nothing  -> throwError "not an object"

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

setProp :: ObjKey -> Expr -> ObjId -> EvalMonad2 ()
setProp key value oid = do
  obj <- get oid
  update oid $ Map.insert key value obj

getProp :: ObjKey -> ObjId -> EvalMonad2 (Maybe Expr)
getProp key oid = do
  obj <- get oid
  return $ Map.lookup key obj


-- | Lookup a variable in an object
lookupVar :: Path -> ObjId -> EvalMonad2 (Maybe Variable)
lookupVar path oid = do
  let (key, mPath) = Path.uncons path 
  obj <- get oid
  let mValue = Map.lookup key obj
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
  let mPrototype = Map.lookup (StrKey "__protos") obj
  case mPrototype of
    Nothing -> return Nothing
    Just (Obj protosId) -> do
      obj' <- get protosId
      doProtoListLookup $ Map.elems obj'
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
        result -> return result
    
    -- | attempt to find a variable within an individual prototype
    doProtoLookup :: Expr -> EvalMonad2 (Maybe Variable)
    doProtoLookup (Obj poid)  = do
      mvar <- lookupVar path poid
      let result =
            case mvar of
              Just var@(ObjVar _ _) -> Just $ ProtoVar var oid path
              Just (ProtoVar var _ _) -> Just $ ProtoVar var oid path
              _ -> Nothing
      return result
    doProtoLookup _ = throwError "__protos item is not Object"




--setProps :: (LiteralType a) => [(String, a)] -> Object -> Object
--setProps props obj = foldr setProp obj props





                
