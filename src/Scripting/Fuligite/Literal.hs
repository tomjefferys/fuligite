{-# LANGUAGE FlexibleInstances #-}
module Scripting.Fuligite.Literal where

import Data.List (find)
import Data.Maybe (fromMaybe)

-- | Represents a literal type
data Literal = B Bool | C Char | S String | I Int | F Float
             deriving (Ord, Eq, Show)

data LitType = L_BOOL | L_CHAR | L_STRING | L_INT | L_FLOAT
             deriving (Ord, Eq, Show)

getType :: Literal -> LitType
getType l =
    case l of 
        (B _) -> L_BOOL
        (C _) -> L_CHAR
        (S _) -> L_STRING
        (I _) -> L_INT
        (F _) -> L_FLOAT

promoteLit :: LitType -> Literal -> Literal
promoteLit t (B b) = promote b t
promoteLit t (C c) = promote c t
promoteLit t (S s) = promote s t
promoteLit t (I i) = promote i t
promoteLit t (F f) = promote f t


class Promotable a where
    promote :: a -> LitType -> Literal

promoteErrStr :: LitType -> LitType -> String
promoteErrStr f t = "Can't promote " ++ show f ++ " to " ++ show t

getCommonType :: LitType -> LitType -> LitType
getCommonType t1 t2 = 
    fromMaybe 
        (error $ show t1 ++ " and " ++ show t2 ++ " have no common type")
        $ find (\e -> e `elem` promotions t2) $ promotions t1


promotions :: LitType -> [LitType]
promotions L_BOOL   = [L_BOOL, L_INT, L_FLOAT, L_STRING]
promotions L_CHAR   = [L_CHAR, L_STRING]
promotions L_STRING = [L_STRING]
promotions L_INT    = [L_INT, L_FLOAT, L_STRING]
promotions L_FLOAT  = [L_FLOAT, L_STRING]

instance Promotable Bool where
    promote b L_BOOL   = B b
    promote b L_INT    = I $ if b then 1 else 0
    promote b L_FLOAT  = F $ if b then 1.0 else 0.0
    promote b L_STRING = S $ show b
    promote _ t        = error $ promoteErrStr L_BOOL t

instance Promotable Char where
    promote c L_CHAR   = C c
    promote c L_STRING = S [c]
    promote _ t        = error $ promoteErrStr L_CHAR t

instance Promotable String where
    promote s L_STRING = S s
    promote _ t        = error $ promoteErrStr L_STRING t

instance Promotable Int where
    promote i L_BOOL   = B $ i /= 0
    promote i L_INT    = I i
    promote i L_FLOAT  = F $ fromIntegral i
    promote i L_STRING = S $ show i
    promote _ t        = error $ promoteErrStr L_INT t

instance Promotable Float where
    promote f L_FLOAT  = F f
    promote f L_STRING = S $ show f
    promote _ t        = error $ promoteErrStr L_FLOAT t


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


