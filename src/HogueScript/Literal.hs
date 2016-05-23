module HogueScript.Literal where

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

