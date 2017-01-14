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
        getEnvId  :: NonEmpty EnvId,
        objCache  :: IdCache Object }

type EnvId = Int

-- | An environment, the gloval env
-- will not have a parentt
type Env = Object

-- Creates a new binding between String and Expr
declareVariable :: String -> Expr -> Env -> Env
declareVariable = PropList.insertNew

getEnvById :: EnvId -> EvalMonad2 Env
getEnvById eid = do
  cache <- objCache <$> get
  return $ IdCache.getValue eid cache

setEnvById :: EnvId -> Env -> EvalMonad2 ()
setEnvById eid env = do
  st <- get
  let cache  = objCache st
  let cache' = IdCache.updateValue eid env cache
  put st { objCache = cache' }


getEnv :: EvalMonad2 Env
getEnv = do
  eid   <- getEnvId <$> get
  cache <- objCache <$> get
  return $ IdCache.getValue (NonEmpty.head eid) cache

setEnv :: Env -> EvalMonad2 ()
setEnv env = do
  st <- get
  let eid    = getEnvId st
  let cache  = objCache st
  let cache' = IdCache.updateValue (NonEmpty.head eid) env cache
  put $ st { objCache = cache' }

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

