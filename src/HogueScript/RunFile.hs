module HogueScript.RunFile where

import HogueScript.Functions (defaultEnv)
import HogueScript.Object (mkObj)
import HogueScript.ObjKey (ObjKey(..))
import HogueScript.Expr (Expr(..), EvalState(..), Object, EvalMonad)
import HogueScript.Eval (makeEvalState, eval, declareVar)
import HogueScript.Literal (toString)
import HogueScript.FileLoader (loadFile)
import qualified HogueScript.Zipper as Zipper

import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as Map

runFile :: String -> IO ()
runFile fileName = do
    object <- loadFile fileName
    runObjectFile object

runObjectFile :: Object -> IO ()
runObjectFile obj = do
    let st = makeEvalState defaultEnv mkObj
    let eResult = runStateT (runObject obj) st
    let (str, st') = case eResult of
                    Left err -> (show err, st)
                    Right (_, st'') -> ("OK", st'')
    print str
    let mOut = getStdOut st'
    case mOut of 
        Just out -> putStrLn out
        _ -> print ""
    
    
runObject :: Object -> EvalMonad ()
runObject obj =
    -- Bind (but not evaluated) named properties
    -- execute numbered properties
    forM_ (Map.toList obj)
        (\item ->
            case item of
              (StrKey key, expr) -> bindExpr key expr
              (NumKey _, expr) -> eval expr
              (NullKey, _) -> error "Unexpected NullKey found")
  where
    bindExpr :: String -> Expr -> EvalMonad Expr
    bindExpr key expr = do
      env <- fmap getEnv get
      declareVar (Zipper.fromList env) key expr


runPropFile :: [(ObjKey, Expr)] -> IO ()
runPropFile props = do
    -- set up the environment
    let env = foldr
                 (\(key,expr) acc -> Map.insert key expr acc) 
                 defaultEnv props

    let mMain = Map.lookup (StrKey "main") env
    case mMain of
      Just expr -> do
        let eResult = runStateT (eval expr) (makeEvalState env mkObj)
        case eResult of
          Left err -> print err
          Right (_,st) -> 
            print $ fromMaybe "" $ getStdOut st

      Nothing -> print "No main"

getStdOut :: EvalState -> Maybe String
getStdOut st = do
    let env = getEnv st
    expr <- case env of
               [] -> Nothing 
               top:_ -> Map.lookup (StrKey "stdout") top
    lit <- case expr of
             Lit l -> Just l
             _ -> Nothing
    return $ toString lit
