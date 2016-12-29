{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
-- | Custom data type to represent a sequence of properties
-- each property may or may not have a key associated with it, 
-- and duplicates are allowed
module HogueScript.PropertyList 
( PropList,
  empty,
  elems,
  add,
  insert,
  insertNew,
  lookup,
  fromList,
  toList,
  Item(..)
) where

import Data.Sequence (Seq, (<|))
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq

import qualified Data.List as List
import Prelude hiding (lookup)

-- | An individual item, is either a value, or a key and value
data Item k v = Value v | KeyValue k v
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The list itsleft
data PropList k v = PropList (Seq (Item k v))
  deriving (Ord, Eq, Show, Functor, Foldable, Traversable)

empty :: PropList k v
empty = PropList Seq.empty 

fromList :: [(k,v)] -> PropList k v
fromList lst = PropList $ Seq.fromList $ map (uncurry KeyValue) lst

toList :: PropList k v -> [Item k v]
toList (PropList items) = Foldable.toList items

-- | Returns a list of all the items
elems :: PropList k v -> [v]
elems = map getItem . toList 

getItem :: Item k v -> v
getItem (Value item) = item
getItem (KeyValue _ item) = item

hasKey :: (Eq k) => k -> Item k v -> Bool
hasKey _ (Value _) = False
hasKey key (KeyValue key' _) = key == key'

add :: v -> PropList k v -> PropList k v
add value (PropList items) = PropList $ Value value <| items

-- | as with Map.insert, this overwrites any pre existing value
insert :: (Eq k) => k -> v -> PropList k v -> PropList k v
insert key value (PropList items) =
    let mIndex = Seq.findIndexL (hasKey key) items
        item = KeyValue key value
    in PropList $
        case mIndex of 
          Just index -> Seq.update index item items
          Nothing -> item <| items

-- | Inserts this property as a new item, shadowing any preexisting
-- item
insertNew :: k -> v -> PropList k v -> PropList k v
insertNew key value (PropList items) =
  PropList $ KeyValue key value <| items

lookup :: (Eq k) => k -> PropList k v -> Maybe v
lookup key (PropList items) = do
  propItem <- List.find (hasKey key) items
  return $ getItem propItem
