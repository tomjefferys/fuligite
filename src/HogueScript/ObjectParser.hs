module HogueScript.ObjectParser where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))
import Data.Map.Strict ()
import qualified Data.Map.Strict as Map

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

--pathIndex :: Parser [String]
--pathIndex = try $ mkLst <$> path <*> ((char '[') *> quotedString <* (char ']'))
--  where
--    mkLst lst extra = lst ++ [extra]
--
--path :: Parser [String]
--path = choice [simplePath] --, pathIndex]

declaration :: Parser Expr
declaration = try $ Decl <$> (string "var" *> spaces *> path)
                        <*> (spaces *> (char '=') *> spaces *> expression)

assignment :: Parser Expr
assignment = try $ Set <$> path
                    <*> (spaces *> (char '=') *> spaces *> expression)

retrieval :: Parser Expr
retrieval = try $ Get <$> path

ifexpr :: Parser Expr
ifexpr = try $ makeif <$> ((string "if") *> spaces *> expression)
                      <*> (spaces *> (string "then") *> spaces *> expression)
                      <*> (spaces *> (string "else") *> spaces *> expression)
  where
    makeif cond expr1 expr2 = If cond expr1 expr2

notexpr :: Parser Expr
notexpr = try $ Not <$> ((string "not") *> spaces *> expression)

failexpr :: Parser Expr
failexpr = try $ Fail <$> ((string "fail") *> spaces *> quotedString)

-- function application
funapp :: Parser Expr
funapp = try $
        Fapp <$> (char '(' *> spaces *> identifier)
             <*> (spaces *> many (expression <* spaces) <* char ')')

-- function declaration
fundec :: Parser Expr
fundec = try $ Fdec <$> ((string "fn") *> spaces *> many (identifier <* spaces))
                    <*> (spaces *> string "->" *> spaces *> expression)

expression :: Parser Expr
expression = choice [object, ifexpr, notexpr, failexpr, litExpr,
                    funapp, fundec, declaration, assignment, retrieval]

propmap :: Parser (String, Expr)
propmap = (,) <$> identifier
                <*> (spaces *> (char ':') *> spaces *> expression)

object :: Parser Expr
object = try $ mkobj <$> ((char '{') *> spaces *> many1 (propmap <* spaces)
                          <* (char '}'))
  where
    mkobj props = Obj $ Map.fromList props

propfile :: Parser [(String, Expr)]
propfile = (spaces) *> (propmap `endBy` (spaces))

-- parseFromFile propfile "data/objects"

