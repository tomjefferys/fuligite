module HogueScript.State where

import HogueScript.Expr
import qualified HogueScript.Environment as Env
import qualified Util.IdCache as IdCache
import qualified Data.List.NonEmpty as NonEmpty

emptyEvalState :: EvalState
emptyEvalState = 
  let (eid, cache) = IdCache.addValue (Env.empty Nothing)
                     $ IdCache.empty "ObjCache"
  in EvalState
      (NonEmpty.fromList [eid])
      cache
  
-- | Constructs an evaluation state
makeEvalState :: Object     -- ^ The environment
              -> EvalState  -- ^ Returns a new EvalState
makeEvalState env =
  let (eid, cache) = IdCache.addValue env
                        $ IdCache.empty "ObjCache"
  in EvalState
        (NonEmpty.fromList [eid])
        cache
