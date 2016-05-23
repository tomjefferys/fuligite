-- | LiteralType instances
{-# LANGUAGE FlexibleInstances #-}
module HogueScript.LiteralType where

import HogueScript.Expr
import HogueScript.Literal
import Control.Monad.State.Strict
import HogueScript.Eval


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
