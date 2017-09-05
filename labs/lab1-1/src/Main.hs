
{-# LANGUAGE MultiWayIf #-}

import Parser
import CompanyParsers

import System.Environment

main :: IO ()
main =
  let ps = defaultParsers
  in do
    args <- getArgs
    if | null args ->
           putStrLn "Must give one argument"
       | length args > 1 ->
           putStrLn "Must give only one file"
       | otherwise -> do
           f <- readFile $ head args
           case parse ps f of
             Left e -> do
               putStr "Error! "
               print e
             Right v ->
               print v
