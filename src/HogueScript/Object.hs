{-# LANGUAGE FlexibleInstances #-}
module HogueScript.Object 
( Object,
  Literal(..),
  mkLit,
  Expr(..),
  evalProp,
  evalPropString,
  defaultProp,
  setProp,
  setProps,
  mkObj,
  defaultEnv,
  makeEvalState,
  eval,
  EvalState(..),
  ObjKey(..),
  ObjKeySrc(..),
  PropError,
  (%)
) where

import Data.Map.Strict (Map)
import Data.Either (either)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Debug.Trace

data Type = BOOL | CHAR | STRING | INT | FLOAT | OBJECT
            deriving (Show, Eq)
data PropError = BAD_TYPE Type --Type
                 | NO_SUCH_PROP ObjKey
                 | TRACE Expr PropError
                 deriving (Show, Eq)

-- Represents a literal type
data Literal = B Bool | C Char | S String | I Int | F Float
             deriving (Ord, Eq, Show)

-- | Gets the string representation of a literal
-- unlike show, returns strings without quotes.
toString :: Literal -> String
toString lit = 
    case lit of 
        B b -> show b
        C c -> show c
        S s -> s
        I i -> show i
        F f -> show f

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

-- function to declare a variable
-- (var name expr)
fnVar :: [Expr] -> EvalMonad Expr
fnVar [name,value] = do
    env <- fmap getEnv get
    obj <- fmap getObject get
    fl <- fmap failure get
    let env' = case name of
            (Get [identifier]) -> Map.insert (StrKey identifier) value env
            _ -> env
    put $ EvalState env' obj fl
    return value


-- Represents components of an expression
data Expr = Lit Literal |
            Obj Object |
            Get [String] |
            Fapp [String] [Expr] | -- ^ Function application
            Fn [String] Expr |   -- ^ Function
            HFn BuiltIn | -- ^ A builtin function
            Null 
            deriving (Ord, Eq, Show)

-- | Represents something that can be coerced into an expression
class LiteralType a where
    getExpr :: a -> Expr
    fromExpr :: Expr -> EvalMonad a

mkLit :: (LiteralType a) => a -> Expr
mkLit lit = getExpr lit

instance LiteralType Expr where
    getExpr e = e
    fromExpr e = return e 

-- | LiteralType instance for chars
instance LiteralType Char where
    getExpr c = Lit $ C $ c
    fromExpr (Lit (C c)) = return c
    fromExpr (Lit _) = lift $ Left $ BAD_TYPE CHAR
    fromExpr expr = eval expr >>= fromExpr

-- | LiteralType instance for Ints
instance LiteralType Int where
    getExpr n = Lit $ I $ n
    fromExpr (Lit (I i)) = return i
    fromExpr (Lit _) = lift $ Left $ BAD_TYPE INT
    fromExpr expr = eval expr >>= fromExpr

-- | LiteralType instance for floats
instance LiteralType Float where
    getExpr f = Lit $ F $ f
    fromExpr (Lit (F i)) = return i
    fromExpr (Lit _) = lift $ Left $ BAD_TYPE FLOAT
    fromExpr expr = eval expr >>= fromExpr

-- | LiteralType instance for booleans
instance LiteralType Bool where
    getExpr b = Lit $ B $ b
    fromExpr (Lit (B b)) = return b
    fromExpr (Lit _) = lift $ Left $ BAD_TYPE BOOL 
    fromExpr expr = eval expr >>= fromExpr

-- | LiteralType instance for Strings (requires FlexibileInstances)
instance LiteralType String where
    getExpr s = Lit $ S $ s
    fromExpr (Lit (S s)) = return s
    fromExpr (Lit _) = lift $ Left $ BAD_TYPE STRING
    fromExpr expr = eval expr >>= fromExpr

instance LiteralType Object where
    getExpr o = Obj o
    fromExpr (Obj o) = return o
    fromExpr (Lit _) = lift $ Left $ BAD_TYPE OBJECT
    fromExpr expr = eval expr >>= fromExpr

data ObjKey = StrKey String | NumKey Integer | NullKey
            deriving (Show, Ord, Eq)

class ObjKeySrc a where
    getKey :: a -> ObjKey

--instance ObjKeySrc Int where
--    getKey n = NumKey n
--

instance ObjKeySrc Integer where
    getKey n = NumKey n

instance ObjKeySrc String where
    getKey str = StrKey str
    

-- The map of properties for an entity
type Object = Map ObjKey Expr

-- A zipper for Objects
-- basic type is ObjZipper [] Object
data ObjZipper = ObjZipper [(Object, ObjKey)] Expr

getZipperExpr :: ObjZipper -> Expr
getZipperExpr (ObjZipper _ expr) = expr

getField :: ObjZipper -> ObjKey -> Either PropError ObjZipper
getField (ObjZipper path expr) field = 
    case expr of
      (Obj obj) -> case Map.lookup field obj of
                     Just result -> Right $ ObjZipper ((obj,field):path) result
                     Nothing -> Left $ NO_SUCH_PROP field
      _ -> Left $ NO_SUCH_PROP field

getPath :: ObjZipper -> [ObjKey] -> Either PropError ObjZipper
getPath zipper [] = Right zipper
getPath zipper (field:path) = do
    zipper' <- getField zipper field
    getPath zipper' path
    

collapse :: ObjZipper -> ObjZipper
collapse (ObjZipper [] expr) = ObjZipper [] expr
collapse (ObjZipper ((obj,field):path) expr) = 
    collapse $
        ObjZipper path $ Obj $ Map.insert field expr obj

-- Try to lookup a variable in an object
--lookup :: [ObjKey] -- ^ The path we want to find
--       -> Object   -- ^ The object to inspect
--       -> Either PropError ObjZipper -- Either error, or Zipper
--lookup path obj = getPath (ObjZipper [] (Obj obj)) path

lookupPath :: [ObjKey] -> EvalMonad (Either PropError ObjZipper)
lookupPath path = do
    obj <- fmap getObject get
    env <- fmap getEnv get
    let objResult = getPath (ObjZipper [] (Obj obj)) path
    let envResult = getPath (ObjZipper [] (Obj env)) path
    return $ either (const envResult) Right objResult

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

    

-- The state of evaluation of a property expression
data EvalState = EvalState {
                    getEnv :: Object,
                    getObject :: Object,
                    failure :: Maybe String }

-- The Monad stack used when evaluating expressions
type EvalMonad = StateT EvalState (Either PropError)

logFailure :: String -> EvalState -> EvalState
logFailure str evalSt = evalSt { failure = Just str }

setPropMap ::Object -> EvalState -> EvalState
setPropMap propMap evalSt = evalSt { getObject = propMap }

makeEvalState :: Object -> Object -> EvalState
makeEvalState env obj = 
    EvalState env obj Nothing

defaultEnv :: Object
defaultEnv = Map.fromList [(StrKey "var", HFn $ BuiltIn "var" fnVar)] --Map.empty

-- | Set a property to an expression.
setPropM :: [ObjKey] -> Expr -> EvalMonad Expr
setPropM (propName:[]) value = do
    propMap <- fmap getObject get
    let oldExpr = Map.findWithDefault Null propName propMap
    let propMap' = Map.insert propName value propMap
    modify (setPropMap propMap')
    return oldExpr

setProp :: (LiteralType a, ObjKeySrc s) => (s, a) -> Object -> Object
setProp (prop,value) obj = 
    Map.insert (getKey prop) (getExpr value) obj

mkObj :: Object
mkObj = Map.empty

-- | Operator for easy construction of objects, acts as an
-- infix setProp, allows for the following
-- mkObj % ("prop1", "value1") % ("prop2", 11 :: Int)
infixl 5 %
(%) :: (LiteralType a, ObjKeySrc s) => Object -> (s, a) -> Object
obj % property = setProp property obj

setProps :: (LiteralType a) => [(String, a)] -> Object -> Object
setProps props obj = foldr setProp obj props

-- | Get the expression associated with a property
getProp :: [ObjKey] -> EvalMonad Expr
getProp props = do
    obj <- fmap getObject get
    lift $ getPropFromObj props $ Obj obj
    --let mVal = Map.lookup prop propMap
    --lift $ case mVal of
    --          Just val -> Right val
    --          Nothing -> Left $ NO_SUCH_PROP prop

getPropFromObj :: [ObjKey] -> Expr -> Either PropError Expr
getPropFromObj (prop:subprops) (Obj obj) = do
    let mVal = Map.lookup prop obj
    val <- case mVal of
            Just val -> Right val
            Nothing -> Left $ NO_SUCH_PROP prop
    case subprops of
      [] -> Right val
      props -> getPropFromObj subprops val
getPropFromObj (prop:subprops) _ =
    Left $ NO_SUCH_PROP prop
getPropFromObj [] _ =
    Left $ NO_SUCH_PROP $ StrKey "[]"


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

eval :: Expr -> EvalMonad Expr
eval expr = 
    case expr of
        --(Get prop) -> getProp (fmap StrKey prop)
        (Get prop) -> do
            result <- lookupPath (fmap StrKey prop)
            return $ case result of
                        Right zipper -> getZipperExpr zipper
                        Left _ -> Null
        (Null) -> return Null
        (Lit l) -> return $ Lit l
        (Obj o) -> return $ Obj o
        (Fapp path expr) -> doFunc (Fapp path expr)
                
