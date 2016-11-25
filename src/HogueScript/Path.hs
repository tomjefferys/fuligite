module HogueScript.Path where

import HogueScript.ObjKey

data Path = Item ObjKey | Path ObjKey Path
  deriving (Show)

-- | Converts a list of ObjKeys into a Path
-- errors if given an empty list
fromList :: [ObjKey] -> Path
fromList []              = error "Can't run Path.fromList on empty list"
fromList [key]           = Item key
fromList (key:remainder) = Path key $ fromList remainder

-- | Converts a Path into a list of ObjKeys
toList :: Path -> [ObjKey]
toList (Item key)      = [key]
toList (Path key path) = key : toList path

-- | Returns the head of the path, and any tail
uncons :: Path -> (ObjKey, Maybe Path)
uncons (Item objKey)      = (objKey, Nothing)
uncons (Path objKey path) = (objKey, Just path)
