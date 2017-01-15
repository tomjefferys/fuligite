module Scripting.Fuligite.FileLoader 
( loadFile
) where

import Scripting.Fuligite.ObjectParser (objectfile)
import Scripting.Fuligite.Expr (Object, Expr(..))
import Text.ParserCombinators.Parsec (parse)

-- | (row,column)
type Coord = (Int, Int)

-- | A Tuple of a character and a coordinate
type CharCoord = (Char, Coord)

col :: Coord -> Int
col (_,c) = c


-- loads a file.
-- removes comments
-- checks indentation rules
-- parses output
loadFile :: String -> IO Object
loadFile fileName = do
    content <- readFile fileName
    let coords  = toCharCoords content
    let coords' = stripComments coords
    let content' = fromCharCoords coords'
    let check = checkPunctuation coords' []
    case check of
      Left err -> error err
      Right _ -> return $ doParse content'

-- | Strip comments (anything after a semicolon)
stripComments :: [CharCoord] -- | the input content
              -> [CharCoord] -- | the content with comment removed
stripComments [] = []
stripComments ((';',(row,_)):remainder) =
    stripComments $ dropWhile isSameRow remainder
  where
    isSameRow (_,(row',_)) = row == row'
stripComments (ch:remainder) = ch:stripComments remainder

-- | gets parsec to do the actual parsing
doParse :: String -> Object
doParse content =
    case parse objectfile "error" content of
        Left err -> error $ show err
        Right (ObjDef obj) -> obj
        Right _ -> error "File did not return object"


-- | Checks punctuation is correct, ie quotes and brackets 
-- are matched, and bracket indentation is correct
checkPunctuation :: [CharCoord]         -- ^ The content to be checked
                 -> [CharCoord]         -- ^ Punctuation stack
                 -> Either String ()    -- ^ Either an error, or nothing if ok
checkPunctuation [] [] = Right ()
checkPunctuation [] (_:_) = Left "Unexpected end of file"
checkPunctuation (cc@(chr,_):chrs) punctuation = do
    punctuation' <- case chr of
                  '\'' -> handleQuote cc punctuation
                  '"'  -> handleQuote cc punctuation
                  '('  -> ifNotQuoted startBracket cc punctuation
                  '{'  -> ifNotQuoted startBracket cc punctuation
                  ')'  -> ifNotQuoted endBracket cc punctuation
                  '}'  -> ifNotQuoted endBracket cc punctuation
                  _    -> Right punctuation
    checkPunctuation chrs punctuation'

-- | Calls the supplied function if there are no quotes on the punctuation
-- stack
ifNotQuoted :: (CharCoord -> [CharCoord] -> Either String [CharCoord])
            -> CharCoord
            -> [CharCoord]
            -> Either String [CharCoord]
ifNotQuoted _ _ puncts@(('\'',_):_) = Right puncts 
ifNotQuoted _ _ puncts@(('"',_):_)  = Right puncts 
ifNotQuoted fn ch puncts            = fn ch puncts

-- | Handles a quote, if it matches the previous quote than remove
-- that quote from the punction stack.
-- If the top of the stack isn't a quote, push the new character
-- Else ignore the character
handleQuote :: CharCoord                  -- ^ The quote character
            -> [CharCoord]                -- ^ The current punctuation stack
            -> Either String [CharCoord]  -- ^ return an error or an updated 
                                          --   punctition stack
handleQuote chCoord [] = Right [chCoord]
handleQuote (ch, loc) puncts@((ch',_):ptail) 
  | ch == ch'         = Right ptail
  | not $ isQuote ch' = Right $ (ch,loc):puncts
  | otherwise         = Right puncts


-- | Returns true if the suplied character is " or '
isQuote :: Char -> Bool
isQuote '\'' = True
isQuote '"'  = True
isQuote _    = False

-- | Checks an open bracket is indented to the right of
-- teh previous open bracket
startBracket :: CharCoord                 -- ^ The bracket character
             -> [CharCoord]               -- ^ The punctuation stack
             -> Either String [CharCoord] -- ^ error, or updated punctuation stack
startBracket bracket [] = Right [bracket]
startBracket (ch,chloc) brackets@((_,bloc):_)
  | col chloc <= col bloc = Left "Bad open" 
  | otherwise = Right $ (ch,chloc):brackets


-- | Checks an end bracket is appropriately matched up
endBracket :: CharCoord                 -- ^ The bracket character
           -> [CharCoord]               -- ^ The punctuation 
           -> Either String [CharCoord] -- ^ error, or updated punctuation stack
endBracket _ [] = Left "End without open"
endBracket (ch,_) ((b,_):btail) = 
    case ch of
      ')' -> if b == '('
                then Right btail
                else Left "Expecting '}' but got ')'"
      '}' -> if b == '{'
                then Right btail
                else Left "Expecting ')' but got '}'"
      _   -> Left $ "Unexpected end bracket char: " ++ show ch
    
-- | Converts a String into a list of CharCoords
toCharCoords :: String -> [CharCoord]
toCharCoords content = 
    let rows = zip (lines content) [0,1..]
    in concatMap getColVals rows

-- | Converts a List of CharCoords into a String
fromCharCoords :: [CharCoord] -> String
fromCharCoords = fromCharCoords' 0
  where
    fromCharCoords' :: Int -> [CharCoord] -> String
    fromCharCoords' _ [] = ""
    fromCharCoords' row chars@((ch,(row',_)):remainder)
      | row == row' = ch : fromCharCoords' row remainder
      | otherwise   = '\n' : fromCharCoords' (row + 1) chars

-- | Convert a String and a row into a list of char coords
getColVals :: (String, Int) -> [CharCoord]
getColVals (str,ln) = zip str $ zip (repeat ln) [0,1..]
