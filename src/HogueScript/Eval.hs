module HogueScript.Eval where

import HogueScript.Expr
import HogueScript.Literal
import HogueScript.ObjZipper
import HogueScript.ObjKey
import HogueScript.Zipper (Zipper)
import qualified HogueScript.Zipper as Zipper
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

-- | Evaluate an expression
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
                Right (ObjZipper _ _ (HFn (BuiltIn _ fn'))) -> fn'
                _ -> error ("unknown function" ++ show path)
    fn args
doFunc _ = error "Can't call doFunc on non function"

-- TODO make this generic
--data EnvZipper = EnvZipper [Object] [Object]
--
--collapseEnv :: EnvZipper -> [Object]
--collapseEnv (EnvZipper [] outer) = outer
--collapseEnv (EnvZipper (env:inner) outer) =
--    collapseEnv $ EnvZipper inner (env:outer)
--
--setEnvObj :: Object -> EnvZipper -> EnvZipper
--setEnvObj obj (EnvZipper inner []) = error "EnvZipper no env selected"
--setEnvObj obj (EnvZipper inner (env:outer)) = EnvZipper inner (obj:outer)

setEnv :: Zipper Object -> StateSetter
setEnv zipper obj st = st { getEnv = Zipper.toList $ Zipper.set zipper obj }

setObj :: StateSetter
setObj obj st = st { getObject = obj }

-- | Lookups up a path
-- Checks in the local object, then in the environment
lookupPath :: [ObjKey] -- ^ The path
           -> EvalMonad (Either PropError ObjZipper)
lookupPath path = do
    obj <- fmap getObject get
    env <- fmap getEnv get
    let objResult = getPath (ObjZipper setObj [] (Obj obj)) path
    -- TODO need to look through all envs until we find a match
    --let envResult = getPath (ObjZipper setEnv [] (Obj $ head env)) path
    let envResult = lookupEnvPath env path
    return $ either (const envResult) Right objResult

lookupEnvPath :: [Object] -> [ObjKey] -> Either PropError ObjZipper
lookupEnvPath envs path = 
    let result = Zipper.find (findFn path) (Zipper.fromList envs)
    in case result of
        Just zipper -> Right zipper
        Nothing -> Left $ NO_SUCH_PATH path
   
findFn :: [ObjKey] -> Zipper Object -> Maybe ObjZipper
findFn path zipper = do
    obj <- Zipper.get zipper
    let result = getPath (ObjZipper (setEnv zipper) [] (Obj obj)) path
    either (const Nothing) Just result


-- | Logs a failure in the state
logFailure :: String    -- ^ The failure message
           -> EvalState -- ^ The current state
           -> EvalState -- ^ The updated state
logFailure str evalSt = evalSt { failure = Just str }

-- | Set a property of the local object to an expression.
setPropM :: [ObjKey] -> Expr -> EvalMonad Expr
setPropM [propName] value = do
    propMap <- fmap getObject get
    let oldExpr = Map.findWithDefault Null propName propMap
    let propMap' = Map.insert propName value propMap
    modify (setObject propMap')
    return oldExpr
setPropM _ _ = error "setPropM must be supplied with a value"

-- | sets the local object
setObject :: Object -> EvalState -> EvalState
setObject propMap evalSt = evalSt { getObject = propMap }

-- | Constructs an evaluation state
makeEvalState :: Object     -- ^ The environment
              -> Object     -- ^ The local object
              -> EvalState  -- ^ Returns a new EvalState
makeEvalState env obj = 
    EvalState [env] obj Nothing

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
