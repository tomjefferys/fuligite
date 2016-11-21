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
import Control.Monad.State.Strict
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
getObj :: ObjId -> EvalMonad2 Object 
getObj oid = do
    cache <- objCache <$> get
    case IdCache.lookup oid cache of 
      Just obj -> return obj
      Nothing  -> throwError "not an object"

setObj :: Object -> EvalMonad2 ObjId
setObj obj = do
  st <- get
  let cache = objCache st
  let (oid, cache') = IdCache.addValue obj cache
  put st { objCache = cache' }
  return oid

-- | Lookup a variable in an object
lookupVar :: Path -> ObjId -> EvalMonad2 (Maybe Variable)
lookupVar path oid = do
  let (key, mPath) = Path.uncons path 
  obj <- getObj oid
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


-- | Lookup a variable in an objects prototype
lookupProtoVar :: Path -> ObjId -> EvalMonad2 (Maybe Variable)
lookupProtoVar path oid = do
  obj <- getObj oid
  let mPrototype = Map.lookup (StrKey "__proto") obj
  case mPrototype of
    Nothing -> return Nothing
    Just expr -> doProtoLookup expr
  where
    doProtoLookup :: Expr -> EvalMonad2 (Maybe Variable)
    doProtoLookup (Obj poid)  = do
      mvar <- lookupVar path poid
      let result =
            case mvar of
              Just var@(ObjVar _ _) -> Just $ ProtoVar var oid path
              Just (ProtoVar var _ _) -> Just $ ProtoVar var oid path
              _ -> Nothing
      return result
    doProtoLookup _ = throwError "__proto is not Object"




--setProps :: (LiteralType a) => [(String, a)] -> Object -> Object
--setProps props obj = foldr setProp obj props





                
