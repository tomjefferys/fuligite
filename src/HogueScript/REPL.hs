module HogueScript.REPL where

import HogueScript.ObjectParser
import HogueScript.Object
import Control.Monad
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
            let result = evaluate st expr 
            case result of
              Left err -> do
                print err
                return st
              Right (expr', newState) -> do
                print expr'
                return newState


    when (line /= "") $ run st'


evaluate :: EvalState -> Expr -> Either String (Expr, EvalState)
evaluate st expr = doEM st (eval expr) 


