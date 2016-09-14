module HogueScript.RunFile where

import qualified HogueScript.REPL as REPL
import HogueScript.Functions (defaultEnv)
import HogueScript.Object (mkObj)
import HogueScript.ObjectParser (objectfile)
import HogueScript.ObjKey (ObjKey(..))
import HogueScript.Expr (Expr(..), EvalState(..), Object, EvalMonad)
import HogueScript.Eval (makeEvalState, eval, declareVar)
import HogueScript.Literal (toString)
import HogueScript.FileLoader (loadFile)
import qualified HogueScript.Zipper as Zipper

import Control.Monad.State.Strict

import qualified Data.Map.Strict as Map

import Text.ParserCombinators.Parsec (parseFromFile)

runFile :: String -> IO ()
runFile fileName = do
    object <- loadFile fileName
    runObjectFile object

    --result <- parseFromFile objectfile fileName
    --case result of
    --  Right (Obj obj) -> do
    --    runObjectFile obj
    --  Left err -> print err

runObjectFile :: Object -> IO ()
runObjectFile obj = do
    let st = makeEvalState defaultEnv mkObj
    let eResult = runStateT (runObject obj) st
    let (str, st') = case eResult of
                    Left error -> (show error, st)
                    Right (_, st') -> ("OK", st')
    print str
    let mOut = getStdOut st'
    case mOut of 
        Just out -> putStrLn out
        _ -> print ""
    
    
runObject :: Object -> EvalMonad ()
runObject obj = do
    -- Bind (but not evaluated) named properties
    -- execute numbered properties
    forM_ (Map.toList obj)
        (\item ->
            case item of
              (StrKey key, expr) -> bindExpr key expr
              (NumKey _, expr) -> eval expr)
  where
    bindExpr :: String -> Expr -> EvalMonad Expr
    bindExpr key expr = do
      env <- fmap getEnv get
      declareVar (Zipper.fromList env) key expr
      
    executeExpr :: Expr -> EvalMonad Expr
    executeExpr expr = eval expr



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
          Right (expr',st) -> do
            print $ maybe "" id (getStdOut st)

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
