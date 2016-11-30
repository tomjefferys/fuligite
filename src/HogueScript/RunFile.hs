module HogueScript.RunFile where

import qualified HogueScript.DefaultState as DS
import HogueScript.Object (mkObj)
import HogueScript.ObjKey (ObjKey(..))
import HogueScript.Expr (Expr(..), Object,
                          EvalMonad2, doEM, makeEvalState)
import HogueScript.Eval (eval, declareVar, lookupPath)
import HogueScript.Literal (toString)
import HogueScript.FileLoader (loadFile)

import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as Map
import HogueScript.Path (Path(..))

runFile :: String -> IO ()
runFile fileName = do
    object <- loadFile fileName
    runObjectFile object

runObjectFile :: Object -> IO ()
runObjectFile obj = do
    let st = DS.new
    --let eResult = runStateT (runObject obj) st
    let eResult = doEM st (runObject obj)
    let (str, st') = case eResult of
                    Left err -> (show err, st)
                    Right (_, st'') -> ("OK", st'')
    print str
    let mOut = doEM st' getStdOut 
    case mOut of 
        Right (Just out, _) -> putStrLn out
        Right _ -> print "Could not get stdout"
        Left err -> print (show err)
    
    
runObject :: Object -> EvalMonad2 ()
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
    bindExpr :: String -> Expr -> EvalMonad2 Expr
    bindExpr = declareVar 


runPropFile :: [(ObjKey, Expr)] -> IO ()
runPropFile props = do
    -- set up the environment
    let env = foldr
                 (\(key,expr) acc -> Map.insert key expr acc) 
                 DS.defaultEnv props

    let mMain = Map.lookup (StrKey "main") env
    case mMain of
      Just expr -> do
        let eResult = doEM (makeEvalState env mkObj) (eval expr)
        case eResult of
          Left err     -> print err
          Right (_,st) -> do
            let mOut = doEM st getStdOut
            case mOut of
              Right (out, _) -> print $ fromMaybe "" out
              Left err       -> print $ show err

      Nothing -> print "No main"

getStdOut :: EvalMonad2 (Maybe String)
getStdOut  = do
    mExpr <- lookupPath (Item $ StrKey "stdout")
    return $ case mExpr of
             Just (Lit l) -> Just $ toString l
             _     -> Nothing
