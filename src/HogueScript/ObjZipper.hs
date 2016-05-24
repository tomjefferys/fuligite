-- | A simple zipper to access properties in nested objects
module HogueScript.ObjZipper where

import HogueScript.Expr
import HogueScript.ObjKey
import qualified Data.Map.Strict as Map

-- A zipper for Objects
-- basic type is ObjZipper [] Object
data ObjZipper = ObjZipper [(Object, ObjKey)] Expr

getZipperExpr :: ObjZipper -> Expr
getZipperExpr (ObjZipper _ expr) = expr

getField :: ObjZipper -> ObjKey -> Either PropError ObjZipper
getField (ObjZipper path expr) field = 
    case expr of
      (Obj obj) -> case Map.lookup field obj of
                     Just result -> Right $ ObjZipper ((obj,field):path) result
                     Nothing -> Left $ NO_SUCH_PROP field
      _ -> Left $ NO_SUCH_PROP field

getPath :: ObjZipper -> [ObjKey] -> Either PropError ObjZipper
getPath zipper [] = Right zipper
getPath zipper (field:path) = do
    zipper' <- getField zipper field
    getPath zipper' path
    

collapse :: ObjZipper -> ObjZipper
collapse (ObjZipper [] expr) = ObjZipper [] expr
collapse (ObjZipper ((obj,field):path) expr) = 
    collapse $
        ObjZipper path $ Obj $ Map.insert field expr obj
