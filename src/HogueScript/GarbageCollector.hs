module HogueScript.GarbageCollector 
(runGC) where

import HogueScript.Expr (EvalState(..), EvalMonad2,
                          ObjId, Expr(..))
import qualified HogueScript.Object as Obj
import Control.Monad.State.Strict
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Util.IdCache as IdCache
import Control.Monad (foldM)
import qualified HogueScript.PropertyList as PropList

-- | Data structure for tri colour mark and sweep, 
-- ultimately this could be stored with the state
-- and run incremntally
data GCState = GCState {
  getWhite :: Set ObjId,    -- ^ Candidates for gc
  getGrey  :: Set ObjId,    -- ^ Objects that still need to be scanned
  getBlack :: Set ObjId     -- ^ Objects that have been scanned
}

-- | Create a new GCState with the provided white items
newGCState :: [ObjId] -> GCState
newGCState newItems = GCState {
    getWhite = Set.fromList newItems,
    getGrey  = Set.empty,
    getBlack = Set.empty
}

-- | Move an item from the white set to the grey set
moveToGrey :: ObjId -> GCState -> GCState
moveToGrey item gcState = 
  if Set.member item $ getWhite gcState
    then GCState {
          getWhite = Set.delete item $ getWhite gcState,
          getGrey  = Set.insert item $ getGrey gcState,
          getBlack = getBlack gcState
    }
    else gcState

-- | Move an item from the grey set to the black set
moveToBlack :: ObjId -> GCState -> GCState
moveToBlack item gcState = 
  if Set.member item $ getGrey gcState
    then GCState {
          getWhite = getWhite gcState,
          getGrey  = Set.delete item $ getGrey gcState,
          getBlack = Set.insert item $ getBlack gcState
    }
    else gcState
  
-- | Perform a full stop the world garbage collection
-- returns number of objects swept
runGC :: EvalMonad2 Int
runGC = do
  st <- setupGCState
  st' <- doScan st
  let numToSweep = Set.size $ getWhite st'
  sweepWhites st'
  return numToSweep

-- | Perform scan until there are no more grey items
doScan :: GCState -> EvalMonad2 GCState
doScan st = do
  st' <- scanGreys st
  if Set.size (getGrey st') > 0
    then doScan st'
    else return st'

-- Adds all objects to the white state
-- moves active Env objects to grey state
setupGCState :: EvalMonad2 GCState
setupGCState = do
  objs <- IdCache.allIds . objCache <$> get
  let gcState = newGCState objs
  eids <- getEnvId <$> get
  return $ foldr moveToGrey gcState eids


-- Examines all the current grey objects
scanGreys :: GCState -> EvalMonad2 GCState
scanGreys gcstate = foldM scanGrey gcstate $ getGrey gcstate
  where
    -- Scan a single grey item, examines each of it's elements.
    scanGrey :: GCState -> ObjId ->  EvalMonad2 GCState
    scanGrey st item = do
      obj <- Obj.get item
      st' <- foldM processItem st $ PropList.elems obj
      return $ moveToBlack item st'
    
    -- Examine an element of a grey item, and mark any objects
    -- or environments encountered as grey
    processItem :: GCState -> Expr -> EvalMonad2 GCState
    processItem st expr = 
      return
        $ case expr of 
          (Obj objId)    -> moveToGrey objId st
          (Fn envId _ _) -> moveToGrey envId st
          _              -> st

-- | Remove all white items, assuming all greys have been
-- scanned, these should be objects no longer referenced
sweepWhites :: GCState -> EvalMonad2 GCState
sweepWhites gcState = do
  mapM_ Obj.delete $ getWhite gcState
  return $ gcState { getWhite = Set.empty }
