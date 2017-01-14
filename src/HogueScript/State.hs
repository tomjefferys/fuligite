module HogueScript.State where

import HogueScript.Expr
import qualified HogueScript.Environment as Env
import qualified Util.IdCache as IdCache
import qualified Data.List.NonEmpty as NonEmpty

emptyEvalState :: EvalState
emptyEvalState = 
  let (eid, cache) = IdCache.addValue (Env.empty Nothing)
                     $ IdCache.empty "EnvCache"
  in EvalState
      cache
      (NonEmpty.fromList [eid])
      (IdCache.empty "ObjCache")
  
-- | Constructs an evaluation state
makeEvalState :: Object     -- ^ The environment
              -> EvalState  -- ^ Returns a new EvalState
makeEvalState env =
  let (eid, envs) = IdCache.addValue env
                        $ IdCache.empty "EnvCache"
  in EvalState
        envs
        (NonEmpty.fromList [eid])
        (IdCache.empty "ObjCache")
