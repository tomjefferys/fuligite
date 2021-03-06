module Scripting.Fuligite.RunFile where

import qualified Scripting.Fuligite.DefaultState as DS
import Scripting.Fuligite.Expr (Expr(..), Object, ObjKey,
                          EvalMonad2, doEM)
import Scripting.Fuligite.Eval (eval, declareVar, lookupPath)
import Scripting.Fuligite.Literal (toString)
import Scripting.Fuligite.FileLoader (loadFile)
import Scripting.Fuligite.State

import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)

import Scripting.Fuligite.Path (Path(..))
import qualified Scripting.Fuligite.PropertyList as PropList
import Scripting.Fuligite.PropertyList (Item(..))

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
    forM_ (PropList.toList obj)
        (\item ->
            case item of
              (KeyValue key expr) -> bindExpr key expr
              (Value expr) -> eval expr)
  where
    bindExpr :: String -> Expr -> EvalMonad2 Expr
    bindExpr = declareVar 


runPropFile :: [(ObjKey, Expr)] -> IO ()
runPropFile props = do
    -- set up the environment
    let env = foldr
                 (\(key,expr) acc -> PropList.insert key expr acc) 
                 DS.defaultEnv props

    let mMain = PropList.lookup "main" env
    case mMain of
      Just expr -> do
        let eResult = doEM (makeEvalState env) (eval expr)
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
    mExpr <- lookupPath (Item "stdout")
    return $ case mExpr of
             Just (Lit l) -> Just $ toString l
             _     -> Nothing
