module HogueScript.REPL where

import HogueScript.ObjectParser
import HogueScript.Object
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Text.ParserCombinators.Parsec


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
          Right exp -> do
            print exp
            let result = evaluate exp st
            case result of
              Left error -> do
                print error
                return st
              Right (expr', newState) -> do
                print expr'
                return newState


    when (line /= "") $ run st'


evaluate :: Expr -> EvalState -> Either PropError (Expr, EvalState)
evaluate expr st = runStateT (eval expr) st
