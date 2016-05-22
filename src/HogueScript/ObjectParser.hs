module HogueScript.ObjectParser where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))
import Data.Map.Strict ()
import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')

import HogueScript.Object

litBool :: Parser Literal
litBool = getBool <$> choice [string "true", string "false"]
  where
    getBool "true" = B True
    getBool "false" = B False
    getBool _ = error "Not boolean primative"

litChar :: Parser Literal
litChar = C <$> (char '\'' *> anyChar <* char '\'')

quotedString :: Parser String
quotedString = (char '"' *> many (noneOf ("\"")) <* char '"')

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
litInt = (I . read) <$> integer <* (notFollowedBy $ char '.')

litFloat :: Parser Literal
litFloat = (F . read) <$> float

literal :: Parser Literal
literal = choice [litBool, litChar, litString, litFloat, litInt]

litExpr :: Parser Expr
litExpr = try $ Lit <$> literal

identifier :: Parser String
identifier = letter <:> many alphaNum

--simplePath :: Parser [String]
--simplePath = try $ (:) <$> identifier <*> many ((char '.') *> identifier)


path = (:) <$> identifier
           <*> many (choice [(char '.') *> identifier, 
                             (char '[' *> quotedString <* (char ']'))])

retrieval :: Parser Expr
retrieval = try $ Get <$> path

-- function application
funapp :: Parser Expr
funapp = try $
        Fapp <$> (char '(' *> spaces *> path)
             <*> (spaces *> many (expression <* spaces) <* char ')')

expression :: Parser Expr
expression = choice [object, litExpr, funapp, retrieval]

-- A Property mapping eg propname : "value"
propmap :: Parser (ObjKey, Expr)
propmap = try $ mkPropMap <$> identifier
                <*> (spaces *> (char ':') *> spaces *> expression)
    where mkPropMap ident expr = (StrKey ident, expr)

-- A Property mapping from number to value eg 4: "value"
propnum :: Parser (ObjKey, Expr)
propnum = try $ mkPropNum <$> integer
                <*> (spaces *> (char ':') *> spaces *> expression)
    where mkPropNum n expr = (NumKey $ read n, expr)

-- A Propery where no key is specified, eg a list
nullprop :: Parser (ObjKey, Expr)
nullprop = try $ ((,) NullKey) <$> expression
    --where mkNullProp expr = (NullKey, expr)

object :: Parser Expr
object = try $ mkObj <$> ((char '{') *> spaces *>
                         many1 ((choice [propmap, propnum, nullprop]) <* spaces)
                          <* (char '}'))
  where
    mkObj props = Obj $ fst
                     $ foldl' (\(map,index) (prop,expr) -> 
                            case prop of
                              NullKey -> (Map.insert (NumKey index) expr map, 
                                            index + 1)
                              NumKey n -> (Map.insert (NumKey n) expr map,
                                            n + 1)
                              StrKey s -> (Map.insert (StrKey s) expr map,
                                            index)) 
                          (Map.empty, 0) props

    --mkobj props = Obj $ Map.fromList props

propfile :: Parser [(ObjKey, Expr)]
propfile = (spaces) *> (propmap `endBy` (spaces))

-- parseFromFile propfile "data/objects"

