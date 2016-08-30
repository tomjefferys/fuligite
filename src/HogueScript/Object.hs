{-# LANGUAGE FlexibleInstances #-}
module HogueScript.Object where

import qualified Data.Map.Strict as Map
import HogueScript.ObjKey
import HogueScript.Expr
import HogueScript.LiteralType

class ObjKeySrc a where
    getKey :: a -> ObjKey

instance ObjKeySrc Integer where
    getKey = NumKey 

instance ObjKeySrc String where
    getKey = StrKey

setProp :: (LiteralType a, ObjKeySrc s) => (s, a) -> Object -> Object
setProp (prop,value) = 
    Map.insert (getKey prop) (getExpr value)

mkObj :: Object
mkObj = Map.empty

-- | Operator for easy construction of objects, acts as an
-- infix setProp, allows for the following
--  mkObj % ("prop1", "value1") % ("prop2", 11 :: Int)
infixl 5 %
(%) :: (LiteralType a, ObjKeySrc s) => Object -> (s, a) -> Object
obj % property = setProp property obj

--setProps :: (LiteralType a) => [(String, a)] -> Object -> Object
--setProps props obj = foldr setProp obj props





                
