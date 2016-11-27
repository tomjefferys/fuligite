module Util.IdCache where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)


-- | Manager type.
-- Holds the current map, and the next id
data IdCache v =
    IdCache {
        getName :: String,
        getMap :: IntMap v,
        getNextId :: Int 
    }

member :: Int -> IdCache v -> Bool
member n manager =
    IntMap.member n $ getMap manager

-- | Makes an empty id cache
empty :: String -- ^ A name, used for error reporting
      -> IdCache v
empty name = IdCache name IntMap.empty 0

-- | Get all the values along with their ids
allValues :: IdCache v -> [(Int, v)]
allValues manager = IntMap.toList $ getMap manager

-- | Adds an value to the manager
-- returns a tuple containing the id assigned to the 
-- new value, and the updated IdManger
addValue :: v -> IdCache v -> (Int, IdCache v)
addValue value manager =
    let idMap = getMap manager
        nextId = getNextId manager
    in (nextId,
         manager { getMap = IntMap.insert nextId value idMap,
                   getNextId = nextId + 1 })


-- | Gets a value from it's id
-- errors if the id does not exist
getValue :: Int -> IdCache v -> v
getValue valId manager = 
    let idMap = getMap manager
        mVal = IntMap.lookup valId idMap
    in fromMaybe (error 
            (getName manager
              ++ " getValue: No such Id: "
              ++ show valId))
            mVal

-- | Similar to Map.lookup
lookup :: Int -> IdCache v -> Maybe v
lookup valId manager =
  let idMap = getMap manager
  in IntMap.lookup valId idMap

    

-- | Updates a value, errors if id does not already exist
updateValue :: Int -> v -> IdCache v -> IdCache v
updateValue valId val manager =
    let idMap = getMap manager
    in if IntMap.member valId idMap
        then manager { getMap = IntMap.insert valId val idMap }
        else error (getName manager
                     ++ "updateValue: No such id: "
                     ++ show valId)

removeValue :: Int -> IdCache v -> IdCache v
removeValue valId manager = 
    let idMap = getMap manager
    in if IntMap.member valId idMap
         then manager { getMap = IntMap.delete valId idMap }
         else error (getName manager
                     ++ "deleteValue; no such id: "
                     ++ show valId)
