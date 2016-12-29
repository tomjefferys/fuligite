{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module HogueScript.Expr where

import HogueScript.Literal
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Identity
import Util.IdCache (IdCache)
import qualified Util.IdCache as IdCache 
import HogueScript.Path (Path)
import qualified HogueScript.Path as Path
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import HogueScript.PropertyList (PropList)
import qualified HogueScript.PropertyList as PropList


type ObjKey = String
-- The map of properties for an entity
-- TODO if this was just an array of values, with optional 
-- keys, we wouldn't need the differet types of ObjKey
type Object = PropList ObjKey Expr

emptyObj :: Object
emptyObj = PropList.empty

-- Should object be part of the environment?
-- Should it be passed through as an implicit self reference tp 
-- the function call
type ObjId = Int

-- Represents components of an expression
data Expr = Lit Literal |
            ObjDef Object | -- FIXME get rid of this?
            Obj ObjId |
            Get [String] |
            Fapp [String] [Expr] | -- ^ Function application
            Fn EnvId [String] Expr |   -- ^ Function
            HFn BuiltIn | -- ^ A builtin function
            Null 
            deriving (Ord, Eq, Show)

getIdentifier :: (MonadError String m)
                  => Expr
                  -> m [String]
getIdentifier expr =
    case expr of
      Get path -> return path
      _ -> throwError $ show $ MSSG $ "Not Get: " ++ show expr


-- Type for a builtin function
-- Takes a name and the function itself
data BuiltIn = BuiltIn String ([Expr] -> EvalMonad2 Expr)

instance Ord BuiltIn where
    compare (BuiltIn name1 _) (BuiltIn name2 _) = 
        compare name1 name2

instance Eq BuiltIn where
    (BuiltIn name1 _) == (BuiltIn name2 _) = name1 == name2 

instance Show BuiltIn where
    show (BuiltIn name _) = name

-- | Variable type, consists of variables owner,
-- and name
data Variable = EnvVar EnvId String
                | ObjVar ObjId String
                | ProtoVar Variable ObjId Path

setVarWithPath :: ObjId -> Path -> Expr -> EvalMonad2 ()
setVarWithPath oid path expr = do
  st <- get
  let cache = objCache st
  let obj = IdCache.getValue oid cache
  let (field, mPath) = Path.uncons path
  let mValue = PropList.lookup field obj
  case (mValue, mPath) of
    -- No path remaining, set the field on this object:
    (_, Nothing) -> 
        let obj' = PropList.insert field expr obj
            cache' = IdCache.updateValue oid obj' cache
        in put st { objCache = cache' }
    -- Path remaining field exists and is object:
    (Just (Obj oid'), Just path') -> setVarWithPath oid' path' expr
    -- Path remaining and field is blank:
    (Nothing, Just path') -> do
        let (oid', cache') = IdCache.addValue emptyObj cache
        let obj' = PropList.insert field (Obj oid') obj
        let cache'' = IdCache.updateValue oid obj' cache'
        put st { objCache = cache'' }
        setVarWithPath oid' path' expr
    -- field is not an object
    (_, _) -> throwError "Field is not an object"


-- The state of evaluation of a property expression
data EvalState =
    EvalState {
        envCache  :: IdCache Env,
        getEnvId  :: NonEmpty EnvId,
        objCache  :: IdCache Object,
        getObject :: Maybe ObjId,
        failure   :: Maybe String }

emptyEvalState :: EvalState
emptyEvalState = 
  let (eid, cache) = IdCache.addValue (emptyEnv Nothing)
                     $ IdCache.empty "EnvCache"
  in EvalState
      cache
      (NonEmpty.fromList [eid])
      (IdCache.empty "ObjCache")
      Nothing Nothing
  
-- | Constructs an evaluation state
makeEvalState :: Object     -- ^ The environment
              -> Object     -- ^ The local object
              -> EvalState  -- ^ Returns a new EvalState
makeEvalState env obj =
  let (eid, envs) = IdCache.addValue (Env Nothing env)
                        $ IdCache.empty "EnvCache"
      (oid, objs) = IdCache.addValue obj
                        $ IdCache.empty "ObjCache"
  in EvalState
        envs
        (NonEmpty.fromList [eid])
        objs
        (Just oid)
        Nothing

type EnvId = Int

-- | An environment, the gloval env
-- will not have a parentt
-- TODO could just be an object with a __PARENT?
-- We wouldn't be able to support type safety
data Env = Env {
  parent :: Maybe EnvId,
  getState  :: Object
}

getParent :: Env -> EvalMonad2 (Maybe Env)
getParent env = do
  let mpid = parent env
  case mpid of
    Just pid -> Just <$> getEnvById pid
    Nothing -> return Nothing



-- Creates a new binding between String and Expr
declareVariable :: String -> Expr -> Env -> Env
declareVariable name value env = 
  let state' = PropList.insertNew name value
                $ getState env
  in env { getState = state' }

-- Envs could be considered like an object
-- Objects parents are not writable though

-- | Make a new empty environment
emptyEnv :: Maybe EnvId -> Env
emptyEnv eid = Env eid emptyObj

getEnvById :: EnvId -> EvalMonad2 Env
getEnvById eid = do
  cache <- envCache <$> get
  return $ IdCache.getValue eid cache

setEnvById :: EnvId -> Env -> EvalMonad2 ()
setEnvById eid env = do
  st <- get
  let cache = envCache st
  let cache' = IdCache.updateValue eid env cache
  put st { envCache = cache' }


getEnv :: EvalMonad2 Env
getEnv = do
  eid <- getEnvId <$> get
  cache <- envCache <$> get
  return $ IdCache.getValue (NonEmpty.head eid) cache

setEnv :: Env -> EvalMonad2 ()
setEnv env = do
  st <- get
  let eid    = getEnvId st
  let cache  = envCache st
  let cache' = IdCache.updateValue (NonEmpty.head eid) env cache
  put $ st { envCache = cache' }

pushObj :: (MonadState EvalState m) => ObjId -> m ()
pushObj oid = do
    st <- get
    put st { getObject = Just oid }

popObj :: EvalMonad2 ()
popObj = do
    st <- get
    put st { getObject = Nothing } 

-- The Monad stack used when evaluating expressions
newtype EvalMonad2 a = EvalMonad2 {
    runEM :: StateT EvalState (ExceptT String Identity) a
} deriving (Functor,
            Applicative,
            Monad,
            MonadState EvalState,
            MonadError String)

evalEM :: EvalState -> EvalMonad2 a -> Either String a
evalEM st fn = runIdentity
               $ runExceptT
               $ evalStateT (runEM fn) st

doEM :: EvalState -> EvalMonad2 a -> Either String (a, EvalState)
doEM st fn = runIdentity
              $ runExceptT
              $ runStateT (runEM fn) st



data Type = BOOL | CHAR | STRING | INT | FLOAT | OBJECT
            deriving (Show, Eq)
data PropError = BAD_TYPE Type --Type
                 | NO_SUCH_PROP ObjKey
                 | NO_SUCH_PATH [ObjKey]
                 | TRACE Expr PropError
                 | MSSG String
                 | INVALID_EXPR Expr String
                 | BAD_ARGS [Expr]
                 deriving (Show, Eq)

