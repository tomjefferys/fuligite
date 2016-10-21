module HogueScript.Expr where

import HogueScript.Literal
import HogueScript.ObjKey
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

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

getObj :: Expr -> Either PropError Object
getObj expr =
    case expr of
      Obj obj -> Right obj
      _ -> Left $ BAD_TYPE OBJECT

getIdentifier :: Expr -> Either PropError [String]
getIdentifier expr =
    case expr of
      Get path -> Right path
      _ -> Left $ MSSG $ "Not Get: " ++ show expr


-- Type for a builtin function
-- Takes a name and the function itself
data BuiltIn = BuiltIn String ([Expr] -> EvalMonad Expr)

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

pushEnv :: EvalMonad ()
pushEnv = do
    st <- get
    put st { getEnv = Map.empty : getEnv st }

popEnv :: EvalMonad ()
popEnv = do
    st <- get
    let st' =
          case getEnv st of
            (_:tl) -> st { getEnv = tl }
            []     -> st
    put st'

pushObj :: Object -> EvalMonad ()
pushObj obj = do
    st <- get
    put st { getObject = obj }

popObj :: EvalMonad ()
popObj = do
    st <- get
    put st { getObject = Map.empty } 

-- The Monad stack used when evaluating expressions
type EvalMonad = StateT EvalState (Either PropError)

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

