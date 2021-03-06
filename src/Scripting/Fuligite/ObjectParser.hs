module Scripting.Fuligite.ObjectParser 
( expression,
  object,
  objectfile,
--  propfile
) where

import Text.ParserCombinators.Parsec
--import Control.Applicative hiding (many, (<|>))
import Data.Foldable (foldl')

import Scripting.Fuligite.Literal
import Scripting.Fuligite.Expr
import qualified Scripting.Fuligite.PropertyList as PropList

litBool :: Parser Literal
litBool = getBool <$> choice [string "true", string "false"]
  where
    getBool "true" = B True
    getBool "false" = B False
    getBool _ = error "Not boolean primative"

litChar :: Parser Literal
litChar = C <$> (char '\'' *> anyChar <* char '\'')

quotedString :: Parser String
quotedString = char '"' *> many (noneOf "\"") <* char '"'

litString :: Parser Literal
litString = S <$> quotedString

-- Numbers, lifted from:
-- https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

number :: Parser String
number = many1 digit

plus :: Parser String
plus = char '+' *> number

minus :: Parser String
minus = char '-' <:> number

integer :: Parser String
integer = try $ plus <|> minus <|> number

float :: Parser String
float = try $ integer <++> (char '.' <:> number)

litInt :: Parser Literal
litInt = (I . read) <$> integer <* notFollowedBy (char '.')

litFloat :: Parser Literal
litFloat = (F . read) <$> float

literal :: Parser Literal
literal = choice [litBool, litChar, litString, litFloat, litInt]

litExpr :: Parser Expr
litExpr = try $ Lit <$> literal

identifierChars :: Parser String
identifierChars = many (choice [alphaNum, char '_', char '+'])

identifier :: Parser String
identifier = choice [letter, char '+', char '_'] <:> identifierChars

--simplePath :: Parser [String]
--simplePath = try $ (:) <$> identifier <*> many ((char '.') *> identifier)

path :: Parser [String]
path = (:) <$> identifier
           <*> many (choice [char '.' *> identifier, 
                             char '[' *> quotedString <* char ']'])

retrieval :: Parser Expr
retrieval = try $ Get <$> path

-- function application
funapp :: Parser Expr
funapp = try $
        Fapp <$> (char '(' *> spaces *> path)
             <*> (spaces *> many (expression <* spaces) <* char ')')

expression :: Parser Expr
expression = choice [object, litExpr, funapp, retrieval]

data PropType = StrKey String | NullKey 

-- A Property mapping eg propname : "value"
propmap :: Parser (PropType, Expr)
propmap = try $ mkPropMap <$> identifier
                <*> (spaces *> char ':' *> spaces *> expression)
    where mkPropMap ident expr = (StrKey ident, expr)

-- A Property mapping from number to value eg 4: "value"
--propnum :: Parser (ObjKey, Expr)
--propnum = try $ mkPropNum <$> integer
--                <*> (spaces *> char ':' *> spaces *> expression)
--    where mkPropNum n expr = (NumKey $ read n, expr)
--



-- A Propery where no key is specified, eg a list
nullprop :: Parser (PropType, Expr)
nullprop = try $ (,) NullKey <$> expression

objectbody :: Parser [(PropType, Expr)]
objectbody = many (choice [propmap, nullprop] <* spaces)

object :: Parser Expr
object = try $ mkObj <$> (char '{' *> spaces *> objectbody <* char '}')

-- FIXME, get rid of NumKey option, and complicated indexing logic
mkObj :: [(PropType, Expr)] -> Expr
mkObj props = ObjDef 
                 $ foldl' (\mp (prop,expr) -> 
                        case prop of
                          NullKey -> PropList.add expr mp
                          StrKey s -> PropList.insert s expr mp) 
                      PropList.empty props

objectfile :: Parser Expr
objectfile = mkObj <$> (spaces *> objectbody)

--propfile :: Parser [(PropType, Expr)]
--propfile = spaces *> (propmap `endBy` spaces)


