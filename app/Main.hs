module Main where

import Data.Lambda.Simple.Parser
import Data.Lambda.Simple.Typing
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "\x03BB> "
  case minput of
    Nothing -> return ()
    Just "quit" -> return ()
    Just input -> do
      printeval input
      loop

printeval :: String -> InputT IO ()
printeval s = case parse s of
  Left parseErr -> outputStrLn $ "Parse error: " ++ show parseErr
  Right exp  -> case typeof exp of
    Left typeErr -> outputStrLn $ "Type error: " ++ typeErr
    Right typ -> outputStrLn $ show exp ++ " :: " ++ show typ
