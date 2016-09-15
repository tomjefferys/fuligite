module HogueScript.FileLoader 
( loadFile
) where

import HogueScript.ObjectParser (objectfile)
import HogueScript.Expr (Object, Expr(..))
import Text.ParserCombinators.Parsec (parseFromFile)

type Coord = (Int, Int)
type CharCoord = (Char, Coord)

col :: Coord -> Int
col (_,c) = c

-- FIXME brackets in ", or ' should be ignored

-- loads a file.
-- removes comments
-- checks indentation rules
-- parses output
loadFile :: String -> IO (Object)
loadFile fileName = do
    content <- readFile fileName
    let coords = getCharCoord content
    let check = checkBrackets coords []
    case check of
      Left err -> error err
      Right _ ->  loadAndParse fileName

loadAndParse :: String -> IO (Object)
loadAndParse fileName = do
    result <- parseFromFile objectfile fileName
    case result of
      Right (Obj obj) -> return obj 
      Right _ -> error "File did not return object"
      Left err -> error $ show err

checkBrackets :: [CharCoord] -> [CharCoord] -> Either String ()
checkBrackets [] [] = Right ()
checkBrackets [] (_:_) = Left "End of file with unclosed brakcet"
checkBrackets ((chr,coords):chrs) brackets = do
    brackets' <- case chr of
                  '(' -> startBracket (chr,coords) brackets
                  '{' -> startBracket (chr,coords) brackets
                  ')' -> endBracket (chr,coords) brackets
                  '}' -> endBracket (chr,coords) brackets
                  _ -> Right brackets
    checkBrackets chrs brackets'

startBracket :: CharCoord -> [CharCoord] -> Either String [CharCoord]
startBracket bracket [] = Right [bracket]
startBracket (ch,chloc) brackets@((_,bloc):_) = 
      if (col chloc) <= (col bloc) 
        then Left "Bad open"
        else Right $ (ch,chloc):brackets

endBracket :: CharCoord -> [CharCoord] -> Either String [CharCoord]
endBracket _ [] = Left $ "End without open"
endBracket (ch,_) brackets@((b,bloc):btail) = 
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
