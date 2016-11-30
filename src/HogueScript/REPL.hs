module HogueScript.REPL where

import HogueScript.ObjectParser
import Control.Monad
import Text.ParserCombinators.Parsec
import HogueScript.Eval (evaluate)
import HogueScript.Expr (Expr(..), EvalState)
import qualified HogueScript.DefaultState as DS

type Result = Either String (Expr, EvalState)

main :: IO ()
main = run DS.new

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

