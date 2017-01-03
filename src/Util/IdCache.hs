module Util.IdCache where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)

-- | Cache type.
-- Holds the current map, and the next id
data IdCache v =
    IdCache {
        getName :: String,
        getMap :: IntMap v,
        getNextId :: Int,
        getFreeIds :: IntSet
    }

member :: Int -> IdCache v -> Bool
member n cache =
    IntMap.member n $ getMap cache

-- | Makes an empty id cache
empty :: String -- ^ A name, used for error reporting
      -> IdCache v
empty name = IdCache name IntMap.empty 0 IntSet.empty

-- | Get all the values along with their ids
allValues :: IdCache v -> [(Int, v)]
allValues cache = IntMap.toList $ getMap cache

allIds :: IdCache v -> [Int]
allIds cache = IntMap.keys $ getMap cache

-- | Adds an value to the cache
-- returns a tuple containing the id assigned to the 
-- new value, and the updated IdManger
addValue :: v -> IdCache v -> (Int, IdCache v)
addValue value cache =
    let (nextId, cache') = getNextAvailableId cache
        idMap = getMap cache'
    in (nextId,
         cache' { getMap = IntMap.insert nextId value idMap})

-- | get the next available id, either from the freeIds set,
-- or from the nextId field
getNextAvailableId :: IdCache v -> (Int, IdCache v)
getNextAvailableId cache = 
  if IntSet.null $ getFreeIds cache
    then 
      let nextId = getNextId cache
      in (nextId, cache { getNextId = nextId + 1})
    else
      let (nextId, freeIds) = IntSet.deleteFindMin
                                $ getFreeIds cache
      in (nextId, cache {getFreeIds = freeIds})

-- | Gets a value from it's id
-- errors if the id does not exist
getValue :: Int -> IdCache v -> v
getValue valId cache = 
    let idMap = getMap cache
        mVal = IntMap.lookup valId idMap
    in fromMaybe (error 
            (getName cache
              ++ " getValue: No such Id: "
              ++ show valId))
            mVal

-- | Similar to Map.lookup
lookup :: Int -> IdCache v -> Maybe v
lookup valId cache =
  let idMap = getMap cache
  in IntMap.lookup valId idMap

    

-- | Updates a value, errors if id does not already exist
updateValue :: Int -> v -> IdCache v -> IdCache v
updateValue valId val cache =
    let idMap = getMap cache
    in if IntMap.member valId idMap
        then cache { getMap = IntMap.insert valId val idMap }
        else error (getName cache
                     ++ "updateValue: No such id: "
                     ++ show valId)

removeValue :: Int -> IdCache v -> IdCache v
removeValue valId cache = 
    let idMap = getMap cache
        freeIds = IntSet.insert valId $ getFreeIds cache
    in if IntMap.member valId idMap
         then 
          let (nextId, freeIds')
               = findNextId (getNextId cache, freeIds)
          in cache { getMap     = IntMap.delete valId idMap,
                     getFreeIds = freeIds',
                     getNextId  = nextId }

         else error (getName cache
                     ++ "deleteValue; no such id: "
                     ++ show valId)

findNextId :: (Int, IntSet) -> (Int, IntSet)
findNextId (nextId, freeIds)  = 
  if IntSet.member (nextId - 1) freeIds
    then findNextId (nextId - 1, IntSet.delete (nextId - 1) freeIds)
    else (nextId, freeIds)



