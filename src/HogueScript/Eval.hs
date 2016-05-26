module HogueScript.Eval where

import HogueScript.Expr
import HogueScript.Literal
import HogueScript.ObjZipper
import HogueScript.ObjKey
--import HogueScript.Object
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

eval :: Expr -> EvalMonad Expr
eval expr = 
    case expr of
        (Get prop) -> do
            result <- lookupPath (fmap StrKey prop)
            return $ case result of
                        Right zipper -> getZipperExpr zipper
                        Left _ -> Null
        (Null) -> return Null
        (Lit l) -> return $ Lit l
        (Obj o) -> return $ Obj o
        (Fapp path args) -> doFunc (Fapp path args)
        _ -> undefined

doFunc :: Expr -> EvalMonad Expr
doFunc (Fapp path args) = do
    -- lookup name, first in obj, then env
    fun <- lookupPath $ fmap StrKey path
    let fn = case fun of
                Right (ObjZipper _ (HFn (BuiltIn _ fn'))) -> fn'
                _ -> undefined

    fn args
doFunc _ = error "Can't call doFunc on non function"

lookupPath :: [ObjKey] -> EvalMonad (Either PropError ObjZipper)
lookupPath path = do
    obj <- fmap getObject get
    env <- fmap getEnv get
    let objResult = getPath (ObjZipper [] (Obj obj)) path
    let envResult = getPath (ObjZipper [] (Obj env)) path
    return $ either (const envResult) Right objResult

logFailure :: String -> EvalState -> EvalState
logFailure str evalSt = evalSt { failure = Just str }

-- | Set a property to an expression.
setPropM :: [ObjKey] -> Expr -> EvalMonad Expr
setPropM [propName] value = do
    propMap <- fmap getObject get
    let oldExpr = Map.findWithDefault Null propName propMap
    let propMap' = Map.insert propName value propMap
    modify (setPropMap propMap')
    return oldExpr
setPropM _ _ = error "setPropM must be supplied with a value"

setPropMap ::Object -> EvalState -> EvalState
setPropMap propMap evalSt = evalSt { getObject = propMap }

makeEvalState :: Object -> Object -> EvalState
makeEvalState env obj = 
    EvalState env obj Nothing



-- | Evaluates a property coercing its value into a string
evalPropString :: [ObjKey]
               -> Object
               -> Object
               -> (Either PropError) String
evalPropString path env obj = 
    evalStateT (getProp path >>= evalToString) (makeEvalState env obj)
  where
    evalToString :: Expr -> EvalMonad String
    evalToString (Lit l) = return $ toString l
    evalToString expr = eval expr >>= evalToString

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

-- | Log a failure.  Not an error in the code, but 
-- an expected failure (eg trying to open an open door)
failExpr :: String -> EvalMonad Expr
failExpr str = do
    modify $ logFailure str
    return Null
