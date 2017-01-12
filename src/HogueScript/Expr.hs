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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import HogueScript.PropertyList (PropList)
import qualified HogueScript.PropertyList as PropList


type ObjKey = String

-- The map of properties for an entity
type Object = PropList ObjKey Expr

emptyObj :: Object
emptyObj = PropList.empty

-- Should it be passed through as an implicit self reference tp 
-- the function call
type ObjId = Int

-- Represents components of an expression
data Expr = Lit Literal
          | ObjDef Object
          | Obj ObjId
          | Get [String]
          | Fapp [String] [Expr]
          | Fn EnvId [String] Expr
          | HFn BuiltIn
          | Null 
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

-- The state of evaluation of a property expression
data EvalState =
    EvalState {
        envCache  :: IdCache Env,
        getEnvId  :: NonEmpty EnvId,
        objCache  :: IdCache Object }

emptyEvalState :: EvalState
emptyEvalState = 
  let (eid, cache) = IdCache.addValue (emptyEnv Nothing)
                     $ IdCache.empty "EnvCache"
  in EvalState
      cache
      (NonEmpty.fromList [eid])
      (IdCache.empty "ObjCache")
  
-- | Constructs an evaluation state
makeEvalState :: Object     -- ^ The environment
              -> EvalState  -- ^ Returns a new EvalState
makeEvalState env =
  let (eid, envs) = IdCache.addValue (Env Nothing env)
                        $ IdCache.empty "EnvCache"
  in EvalState
        envs
        (NonEmpty.fromList [eid])
        (IdCache.empty "ObjCache")

type EnvId = Int

-- | An environment, the gloval env
-- will not have a parentt
-- TODO could just be an object with a __PARENT?
-- We wouldn't be able to support type safety
data Env = Env {
  parent   :: Maybe EnvId,
  getState :: Object
}

getParent :: Env -> EvalMonad2 (Maybe Env)
getParent env = do
  let mpid = parent env
  case mpid of
    Just pid -> Just <$> getEnvById pid
    Nothing  -> return Nothing

-- Creates a new binding between String and Expr
declareVariable :: String -> Expr -> Env -> Env
declareVariable name value env = 
  let state' = PropList.insertNew name value
               $ getState env
  in env { getState = state' }

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
  let cache  = envCache st
  let cache' = IdCache.updateValue eid env cache
  put st { envCache = cache' }


getEnv :: EvalMonad2 Env
getEnv = do
  eid   <- getEnvId <$> get
  cache <- envCache <$> get
  return $ IdCache.getValue (NonEmpty.head eid) cache

setEnv :: Env -> EvalMonad2 ()
setEnv env = do
  st <- get
  let eid    = getEnvId st
  let cache  = envCache st
  let cache' = IdCache.updateValue (NonEmpty.head eid) env cache
  put $ st { envCache = cache' }

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
data PropError = BAD_TYPE Type
               | NO_SUCH_PROP ObjKey
               | NO_SUCH_PATH [ObjKey]
               | TRACE Expr PropError
               | MSSG String
               | INVALID_EXPR Expr String
               | BAD_ARGS [Expr]
               deriving (Show, Eq)

