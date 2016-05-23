module HogueScript.Eval where

import HogueScript.Expr
import HogueScript.Literal
import HogueScript.ObjZipper
import HogueScript.ObjKey
import HogueScript.Object
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
        (Fapp path expr) -> doFunc (Fapp path expr)

doFunc :: Expr -> EvalMonad Expr
doFunc (Fapp path expr) = do
    -- lookup name, first in obj, then env
    fun <- lookupPath $ fmap StrKey path
    let fn = case fun of
                Right (ObjZipper _ (HFn (BuiltIn _ fn))) -> fn
                _ -> undefined

    fn expr
    --result <- fn expr
    --return result
    --return Null

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
setPropM (propName:[]) value = do
    propMap <- fmap getObject get
    let oldExpr = Map.findWithDefault Null propName propMap
    let propMap' = Map.insert propName value propMap
    modify (setPropMap propMap')
    return oldExpr

setPropMap ::Object -> EvalState -> EvalState
setPropMap propMap evalSt = evalSt { getObject = propMap }

makeEvalState :: Object -> Object -> EvalState
makeEvalState env obj = 
    EvalState env obj Nothing


-- | Evaluates a property and returns it's value, using 
-- the supplied getter
evalProp :: (LiteralType a)
         => [ObjKey] -- ^ The name of the property
         -> Object -- ^ The environment
         -> Object -- ^ The object to evaluate
         -> (Either PropError) a -- ^ The result
evalProp path env obj =
    evalStateT ((getProp path) >>= fromExpr) (makeEvalState env obj)

-- | Evaluates a property coercing its value into a string
evalPropString :: [ObjKey]
               -> Object
               -> Object
               -> (Either PropError) String
evalPropString path env obj = 
    evalStateT ((getProp path) >>= evalToString) (makeEvalState env obj)
  where
    evalToString :: Expr -> EvalMonad String
    evalToString (Lit l) = return $ toString l
    evalToString expr = eval expr >>= evalToString

defaultProp :: (LiteralType a)
            => a
            -> [ObjKey]
            -> Object
            -> Object
            -> a
defaultProp def path env obj = 
    let result = evalProp path env obj 
    in case result of 
        Right val -> val
        Left _ -> def



-- | Log a failure.  Not an error in the code, but 
-- an expected failure (eg trying to open an open door)
failExpr :: String -> EvalMonad Expr
failExpr str = do
    modify $ logFailure str
    return Null
