module Main where

import qualified Scripting.Fuligite.REPL as REPL
import Scripting.Fuligite.RunFile
import System.Environment (getArgs)

-- | Main function, check if we've been suppled a file
-- if so run that, else drop into the repl
main :: IO ()
main = do
    args <- getArgs
    if null args
      then REPL.main
      else runFile $ head args

--runFile :: String -> IO ()
--runFile fileName = do
--    result <- parseFromFile objectfile fileName
--    case result of
--      Right props -> do
--        runPropFile props
--      Left err -> print err
--
--runObjectFile :: Object -> IO ()
--
--runPropFile :: [(ObjKey, Expr)] -> IO ()
--runPropFile props = do
--    -- set up the environment
--    let env = foldr
--                 (\(key,expr) acc -> Map.insert key expr acc) 
--                 defaultEnv props
--
--    let mMain = Map.lookup (StrKey "main") env
--    case mMain of
--      Just expr -> do
--        let eResult = runStateT (eval expr) (makeEvalState env mkObj)
--        case eResult of
--          Left err -> print err
--          Right (expr',st) -> do
--            print $ maybe "" id (getStdOut st)
--
--      Nothing -> print "No main"
--
--getStdOut :: EvalState -> Maybe String
--getStdOut st = do
--    let env = getEnv st
--    expr <- case env of
--               [] -> Nothing 
--               top:_ -> Map.lookup (StrKey "stdout") top
--    lit <- case expr of
--             Lit l -> Just l
--             _ -> Nothing
--    return $ toString lit

