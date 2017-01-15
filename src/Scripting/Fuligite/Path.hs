module Scripting.Fuligite.Path where

data Path = Item String | Path String Path
  deriving (Show)

-- | Converts a list of ObjKeys into a Path
-- errors if given an empty list
fromList :: [String] -> Path
fromList []              = error "Can't run Path.fromList on empty list"
fromList [key]           = Item key
fromList (key:remainder) = Path key $ fromList remainder

-- | Converts a Path into a list of ObjKeys
toList :: Path -> [String]
toList (Item key)      = [key]
toList (Path key path) = key : toList path

-- | Returns the head of the path, and any tail
uncons :: Path -> (String, Maybe Path)
uncons (Item key)      = (key, Nothing)
uncons (Path key path) = (key, Just path)
