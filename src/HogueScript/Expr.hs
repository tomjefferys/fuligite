{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module HogueScript.Expr where

import HogueScript.Literal
import HogueScript.ObjKey
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Identity

-- The map of properties for an entity
type Object = Map ObjKey Expr


-- Represents components of an expression
data Expr = Lit Literal |
            Obj Object |
            Get [String] |
            Fapp [String] [Expr] | -- ^ Function application
            Fn [String] Expr |   -- ^ Function
            HFn BuiltIn | -- ^ A builtin function
            Null 
            deriving (Ord, Eq, Show)

getObj :: (MonadError String m) 
            => Expr
            -> m Object 
--Either PropError Object
getObj expr =
    case expr of
      Obj obj -> return obj
      _ -> throwError $ show $ BAD_TYPE OBJECT

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

-- The state of evaluation of a property expression
data EvalState =
    EvalState {
        getEnv :: [Object],
        getObject :: Object,
        failure :: Maybe String }

pushEnv :: EvalMonad2 ()
pushEnv = do
    st <- get
    put st { getEnv = Map.empty : getEnv st }

popEnv :: EvalMonad2 ()
popEnv = do
    st <- get
    let st' =
          case getEnv st of
            (_:tl) -> st { getEnv = tl }
            []     -> st
    put st'

pushObj :: (MonadState EvalState m) => Object -> m ()
pushObj obj = do
    st <- get
    put st { getObject = obj }

popObj :: EvalMonad2 ()
popObj = do
    st <- get
    put st { getObject = Map.empty } 

-- The Monad stack used when evaluating expressions
--type EvalMonad = StateT EvalState (Either PropError)

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

