module HogueScript.Expr where

import HogueScript.Literal
import HogueScript.ObjKey
import Data.Map.Strict
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
data EvalState = EvalState {
                    getEnv :: Object,
                    getObject :: Object,
                    failure :: Maybe String }

-- The Monad stack used when evaluating expressions
type EvalMonad = StateT EvalState (Either PropError)

data Type = BOOL | CHAR | STRING | INT | FLOAT | OBJECT
            deriving (Show, Eq)
data PropError = BAD_TYPE Type --Type
                 | NO_SUCH_PROP ObjKey
                 | TRACE Expr PropError
                 deriving (Show, Eq)

-- | Represents something that can be coerced into an expression
class LiteralType a where
    getExpr :: a -> Expr
    fromExpr :: Expr -> EvalMonad a
