module Main where

import qualified HogueScript.REPL as REPL
import HogueScript.Functions (defaultEnv)
import HogueScript.Object (mkObj)
import HogueScript.ObjectParser (propfile)
import HogueScript.ObjKey (ObjKey(..))
import HogueScript.Expr (Expr)
import HogueScript.Eval (makeEvalState, eval)
import System.Environment (getArgs)

import Control.Monad.State.Strict

import qualified Data.Map.Strict as Map

import Text.ParserCombinators.Parsec (parseFromFile)

-- | Main function, check if we've been suppled a file
-- if so run that, else drop into the repl
main :: IO ()
main = do
    args <- getArgs
    if null args
      then REPL.main
      else runFile $ head args

runFile :: String -> IO ()
runFile fileName = do
    result <- parseFromFile propfile fileName
    case result of
      Right props -> do
        runPropFile props
      Left err -> print err

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
          Right (expr',st) -> print expr'
      Nothing -> print "No main"

