-- agda code write some
-- simple hello world

module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Hello, " ++ args !! 0


