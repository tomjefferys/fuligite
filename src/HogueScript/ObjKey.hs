module HogueScript.ObjKey where

data ObjKey = StrKey String | NumKey Integer | NullKey
            deriving (Show, Ord, Eq)
