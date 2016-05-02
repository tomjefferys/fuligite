module HogueScript.REPL where

import HogueScript.ObjectParser
import HogueScript.Object
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Text.ParserCombinators.Parsec


main :: IO ()
main = run mkObj

run :: Object -> IO ()
run obj = do
    line <- getLine
    obj' <- 
        case parse expression "NOPE" line of
          Left err -> do
            print err
            return obj
          Right exp -> do
            print exp
            let result = evaluate exp obj
            case result of
              Left error -> do
                print error
                return obj
              Right (expr', st') -> do
                print expr'
                return $ getObject st'


    when (line /= "") $ run obj'


evaluate :: Expr -> Object -> Either PropError (Expr, EvalState)
evaluate expr obj = runStateT (eval expr) (makeEvalState obj)
