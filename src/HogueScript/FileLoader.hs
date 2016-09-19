module HogueScript.FileLoader 
( loadFile
) where

import HogueScript.ObjectParser (objectfile)
import HogueScript.Expr (Object, Expr(..))
import Text.ParserCombinators.Parsec (parseFromFile)

import Debug.Trace

type Coord = (Int, Int)
type CharCoord = (Char, Coord)

col :: Coord -> Int
col (_,c) = c

-- TODO comments, semicolon seems to be lisp style

-- loads a file.
-- removes comments
-- checks indentation rules
-- parses output
loadFile :: String -> IO (Object)
loadFile fileName = do
    content <- readFile fileName

    let coords = getCharCoord content
    let coords' = trace (show coords) (stripComments coords)
    let check = checkBrackets coords' []
    case check of
      Left err -> error err
      Right _ ->  loadAndParse fileName

stripComments :: [CharCoord] -> [CharCoord]
stripComments [] = []
stripComments ((';',(row,_)):remainder) =
    stripComments $ dropWhile (\(_,(row',_)) -> row == row') remainder
stripComments (ch:remainder) = ch:(stripComments remainder)

loadAndParse :: String -> IO (Object)
loadAndParse fileName = do
    result <- parseFromFile objectfile fileName
    case result of
      Right (Obj obj) -> return obj 
      Right _ -> error "File did not return object"
      Left err -> error $ show err

checkBrackets :: [CharCoord] -> [CharCoord] -> Either String ()
checkBrackets [] [] = Right ()
checkBrackets [] (_:_) = Left "Unexpected end of file"
checkBrackets (cc@(chr,_):chrs) punctuation = do
    punctuation' <- case chr of
                  '\'' -> handleQuote cc punctuation
                  '"' -> handleQuote cc punctuation
                  '(' -> ifNotQuoted startBracket cc punctuation
                  '{' -> ifNotQuoted startBracket cc punctuation
                  ')' -> ifNotQuoted endBracket cc punctuation
                  '}' -> ifNotQuoted endBracket cc punctuation
                  _ -> Right punctuation
    checkBrackets chrs punctuation'

-- | Calls the supplied function if there are no quotes on the punctuation
-- stack
ifNotQuoted :: (CharCoord -> [CharCoord] -> Either String [CharCoord])
            -> CharCoord
            -> [CharCoord]
            -> Either String [CharCoord]
ifNotQuoted _ _ puncts@(('\'',_):_) = Right puncts 
ifNotQuoted _ _ puncts@(('"',_):_) = Right puncts 
ifNotQuoted fn ch puncts = fn ch puncts

-- | Handles a quote, if it matches the previous quote than remove
-- that quote from the punction stack.
-- If the top of the stack isn't a quote, push the new character
-- Else ignore the character
handleQuote :: CharCoord                  -- ^ The quote character
            -> [CharCoord]                -- ^ The current punctuation stack
            -> Either String [CharCoord]  -- ^ return an error or an updated 
                                          --   punctition stack
handleQuote chCoord [] = Right [chCoord]
handleQuote (ch, loc) puncts@((ch',_):ptail) = 
    if ch == ch'
      then Right ptail
      else if not $ isQuote ch'
             then Right $ (ch,loc):puncts
             else Right puncts


-- | Returns true if the suplied character is " or '
isQuote :: Char -> Bool
isQuote '\'' = True
isQuote '"' = True
isQuote _ = False

startBracket :: CharCoord -> [CharCoord] -> Either String [CharCoord]
startBracket bracket [] = Right [bracket]
startBracket (ch,chloc) brackets@((_,bloc):_) = 
      if (col chloc) <= (col bloc) 
        then Left "Bad open"
        else Right $ (ch,chloc):brackets

endBracket :: CharCoord -> [CharCoord] -> Either String [CharCoord]
endBracket _ [] = Left $ "End without open"
endBracket (ch,_) ((b,_):btail) = 
    case ch of
      ')' -> if b == '('
                then Right btail
                else Left "Expecting '}' but got ')'"
      '}' -> if b == '{'
                then Right btail
                else Left "Expecting ')' but got '}'"
      _ -> Left $ "Unexpected end bracket char: " ++ show ch
    
getCharCoord :: String -> [CharCoord]
getCharCoord content = 
    let rows = zip (lines content) [0,1..]
    in concatMap getColVals rows


getColVals :: (String, Int) -> [CharCoord]
getColVals (str,ln) = zip str $ zip (repeat ln) [0,1..]
