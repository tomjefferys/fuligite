{-# LANGUAGE FlexibleInstances #-}
module HogueScript.Object where

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import HogueScript.ObjKey
import HogueScript.Expr

class ObjKeySrc a where
    getKey :: a -> ObjKey

instance ObjKeySrc Integer where
    getKey = NumKey 

instance ObjKeySrc String where
    getKey = StrKey
    

--setProp :: (LiteralType a, ObjKeySrc s) => (s, a) -> Object -> Object
--setProp (prop,value) obj = 
--    Map.insert (getKey prop) (getExpr value) obj

mkObj :: Object
mkObj = Map.empty

-- | Operator for easy construction of objects, acts as an
-- infix setProp, allows for the following
-- mkObj % ("prop1", "value1") % ("prop2", 11 :: Int)
--infixl 5 %
--(%) :: (LiteralType a, ObjKeySrc s) => Object -> (s, a) -> Object
--obj % property = setProp property obj

--setProps :: (LiteralType a) => [(String, a)] -> Object -> Object
--setProps props obj = foldr setProp obj props

-- | Get the expression associated with a property
getProp :: [ObjKey] -> EvalMonad Expr
getProp props = do
    obj <- fmap getObject get
    lift $ getPropFromObj props $ Obj obj

getPropFromObj :: [ObjKey] -> Expr -> Either PropError Expr
getPropFromObj (prop:subprops) (Obj obj) = do
    let mVal = Map.lookup prop obj
    val <- case mVal of
            Just val -> Right val
            Nothing -> Left $ NO_SUCH_PROP prop
    case subprops of
      [] -> Right val
      _ -> getPropFromObj subprops val
getPropFromObj (prop:_) _ =
    Left $ NO_SUCH_PROP prop
getPropFromObj [] _ =
    Left $ NO_SUCH_PROP $ StrKey "[]"



                
