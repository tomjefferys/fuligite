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
        (Obj o) -> 
          pushObj o *> (Obj <$> mapM eval o) <* popObj
        (Fapp path args) -> doFunc path args
        e -> return e


-- | Execute a function
doFunc :: [String]          -- ^ The path to the function
       -> [Expr]            -- ^ The function arguments
       -> EvalMonad Expr
doFunc path args = do
    -- lookup name, first in obj, then env
    fun <- lookupPath $ fmap StrKey path
    let fn = case fun of
                Right (ObjZipper _ _ (HFn (BuiltIn _ fn'))) -> fn'
                Right (ObjZipper _ _ (Fn params def)) -> userFunc params def
                _ -> error ("unknown function" ++ show path)
    pushEnv 
    result <- fn args
    popEnv
    return result

-- | Execute a user defined function
userFunc :: [String] -> Expr -> [Expr] -> EvalMonad Expr
userFunc params expr args = do

    -- bind arguments (When are arguments evaluated?)
    evalArgs <- mapM eval args
    let zipped = zip params evalArgs
    mapM_ dv zipped

    -- execute expr
    eval expr 

  where
    dv :: (String,Expr) -> EvalMonad Expr 
    dv (param,arg) = do
      st <- get
      declareVar (Zipper.fromList $ getEnv st) param arg

-- function to declare a variable
-- (var name expr)
declareVar :: Zipper Object -- ^ A zipper to the environment
           -> String -- ^ The variable neme
           -> Expr   -- ^ The value of this variable
           -> EvalMonad Expr
declareVar envZip name value = do
    value' <- eval value
    let setVar = Map.insert (StrKey name) value'

    st <- get
    -- shift the zipper right as we need to set the variable in the
    -- super environment
    -- for binding of params we don't want to do this
    --let envZip = Zipper.right $ Zipper.fromList $ getEnv st
    let envs =  maybe
                 (getEnv st)
                 (Zipper.toList . Zipper.set envZip . setVar)
                 (Zipper.get envZip)
    put $ st { getEnv =  envs }
    return value'

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
    let envResult = lookupEnvPath env path
    return $ either (const envResult) Right objResult

-- | Lookups up a path in a list of environments
lookupEnvPath :: [Object] -> [ObjKey] -> Either PropError ObjZipper
lookupEnvPath envs path = 
    let result = Zipper.find findFn (Zipper.fromList envs)
    in case result of
        Just zipper -> Right zipper
        Nothing -> Left $ NO_SUCH_PATH path
  where 
    findFn :: Zipper Object -> Maybe ObjZipper
    findFn zipper = do
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
