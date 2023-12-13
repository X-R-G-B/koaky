{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

import Computing.ComputeAST
import Parsing.Parser
import Types
import System.IO (hPutStrLn, stderr, isEOF)
import Debug.Trace

printErrors :: (Env) -> IO ()
printErrors (Env defines_ []) =
  printErrors (Env defines_ ["Unable to compute"])
printErrors (Env _ errors_) = mapM_ (hPutStrLn stderr) errors_

checkComputing :: (Env, Maybe Result) -> IO ()
checkComputing (env, Nothing) = printErrors env
checkComputing (env, Just result) = putStrLn (show result) >> handleInput env

checkParsing :: Maybe (Tree, String) -> Env -> IO ()
checkParsing Nothing _ = hPutStrLn stderr "Unable to parse"
checkParsing (Just (tree, _)) env = trace (show tree ++ " env: " ++ show env) $ checkComputing (computeAST env tree)

checkInput :: String -> Env -> IO ()
checkInput ":q" _ = return ()
checkInput input env = checkParsing (runParser (parseTree) input) env

checkEOF :: Env -> Bool -> IO ()
checkEOF _ True = return ()
checkEOF env False = getLine >>= (\x -> checkInput x env)

handleInput :: Env -> IO ()
handleInput env = isEOF >>= (\x -> checkEOF env x)

main :: IO ()
main = handleInput (Env [] [])
