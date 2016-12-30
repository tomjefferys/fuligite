-- | LiteralType instances
{-# LANGUAGE FlexibleInstances #-}
module HogueScript.LiteralType where

import HogueScript.Expr
import HogueScript.Literal
import Control.Monad.Except
import HogueScript.Eval

-- | Represents something that can be coerced into an expression
class LiteralType a where
    getExpr :: a -> Expr
    fromExpr :: Expr -> EvalMonad2 a

mkLit :: (LiteralType a) => a -> Expr
mkLit = getExpr 

instance LiteralType Expr where
    getExpr e = e
    fromExpr = return 

-- | LiteralType instance for chars
instance LiteralType Char where
    getExpr c = Lit $ C c
    fromExpr (Lit (C c)) = return c
    fromExpr (Lit _) = throwError $ show $ BAD_TYPE CHAR
    fromExpr expr = eval expr >>= fromExpr

-- | LiteralType instance for Ints
instance LiteralType Int where
    getExpr n = Lit $ I n
    fromExpr (Lit (I i)) = return i
    fromExpr (Lit _) = throwError $ show $ BAD_TYPE INT
    fromExpr expr = eval expr >>= fromExpr

-- | LiteralType instance for floats
instance LiteralType Float where
    getExpr f = Lit $ F f
    fromExpr (Lit (F i)) = return i
    fromExpr (Lit _) = throwError $ show $ BAD_TYPE FLOAT
    fromExpr expr = eval expr >>= fromExpr

-- | LiteralType instance for booleans
instance LiteralType Bool where
    getExpr b = Lit $ B b
    fromExpr (Lit (B b)) = return b
    fromExpr (Lit _) = throwError $ show $ BAD_TYPE BOOL 
    fromExpr expr = eval expr >>= fromExpr

-- | LiteralType instance for Strings (requires FlexibileInstances)
instance LiteralType String where
    getExpr s = Lit $ S s
    fromExpr (Lit (S s)) = return s
    fromExpr (Lit _) = throwError $ show $ BAD_TYPE STRING
    fromExpr expr = eval expr >>= fromExpr

-- FIXME does object need to be a literal type
--instance LiteralType Object where
--    getExpr = Obj
--    fromExpr (Obj o) = return o
--    fromExpr (Lit _) = throwError $ show $ BAD_TYPE OBJECT
--    fromExpr expr = eval expr >>= fromExpr

-- | Evaluates a property and returns it's value, using 
-- the supplied getter
--evalProp :: (LiteralType a)
--         => [ObjKey] -- ^ The name of the property
--         -> Object -- ^ The environment
--         -> Object -- ^ The object to evaluate
--         -> (Either String) a -- ^ The result
--evalProp path env obj =
--  evalEM (makeEvalState env obj) (getProp path >>= fromExpr)

--defaultProp :: (LiteralType a)
--            => a
--            -> [ObjKey]
--            -> Object
--            -> Object
--            -> a
--defaultProp def path env obj = 
--    let result = evalProp path env obj 
--    in case result of 
--        Right val -> val
--        Left _ -> def


