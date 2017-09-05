module Main where

import System.Environment
import Path
import Control.Concurrent.STM

main :: IO ()
main = do
  args <- getArgs
  paths <- mapM parseRelFile args
  putStrLn "hello world"
