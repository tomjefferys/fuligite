module Scripting.Fuligite.REPL where

import Scripting.Fuligite.ObjectParser
import Control.Monad
import Text.ParserCombinators.Parsec
import Scripting.Fuligite.Eval (evaluate)
import Scripting.Fuligite.Expr (Expr(..), EvalState)
import qualified Scripting.Fuligite.DefaultState as DS

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

