module HogueScript.REPL where

import HogueScript.ObjectParser
import HogueScript.Object
import Control.Monad
import Control.Monad.State.Strict
import Text.ParserCombinators.Parsec
import HogueScript.Eval
import HogueScript.Expr
import HogueScript.Functions


main :: IO ()
main = run $ makeEvalState defaultEnv mkObj 

run :: EvalState -> IO ()
run st = do
    line <- getLine
    st' <- 
        case parse expression "NOPE" line of
          Left err -> do
            print err
            return st
          Right expr -> do
            print expr
            let result = evaluate expr st
            case result of
              Left err -> do
                print err
                return st
              Right (expr', newState) -> do
                print expr'
                return newState


    when (line /= "") $ run st'


evaluate :: Expr -> EvalState -> Either PropError (Expr, EvalState)
evaluate expr = runStateT (eval expr) 


