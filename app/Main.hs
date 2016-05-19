module Main where

import Parser
import Interpreter

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseProlog line
  case res of
    Left err -> putStrLn $ show err
    Right ex -> case eval ex of
      Nothing -> putStrLn "error"
      Just value -> putStrLn $ show value

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    zinput <- getInputLine "h ~ "
    case zinput of
      Nothing -> outputStrLn "Bye"
      Just input -> (liftIO $ process input) >> loop
