-- | A simple zipper to access properties in nested objects
module HogueScript.ObjZipper where

import HogueScript.Expr
import HogueScript.ObjKey
import qualified Data.Map.Strict as Map

type StateSetter = Object -> EvalState -> EvalState

-- A zipper for Objects
-- basic type is ObjZipper [] Object
data ObjZipper = ObjZipper
                     StateSetter
                     [(Object, ObjKey)] Expr

getZipperExpr :: ObjZipper -> Expr
getZipperExpr (ObjZipper _ _ expr) = expr

setZipperExpr :: ObjZipper -> Expr -> ObjZipper
setZipperExpr (ObjZipper setter path _) = ObjZipper setter path 

getField :: ObjZipper -> ObjKey -> Either PropError ObjZipper
getField (ObjZipper updater path expr) field = 
    case expr of
      (ObjDef obj) -> case Map.lookup field obj of
                     Just result -> Right
                             $ ObjZipper updater ((obj,field):path) result
                     Nothing -> Left $ NO_SUCH_PROP field
      _ -> Left $ NO_SUCH_PROP field

getPath :: ObjZipper -> [ObjKey] -> Either PropError ObjZipper
getPath zipper [] = Right zipper
getPath zipper (field:path) = do
    zipper' <- getField zipper field
    getPath zipper' path
    

collapse :: ObjZipper -> (StateSetter, Expr)
collapse (ObjZipper updater [] expr) = (updater,expr)
collapse (ObjZipper updater ((obj,field):path) expr) = 
    collapse $
        ObjZipper updater path $ ObjDef $ Map.insert field expr obj
