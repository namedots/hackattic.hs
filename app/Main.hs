module Main (main) where

import Protolude

main :: IO ()
main = do
  putStrLn "You have found the entry point of this project."
  putStrLn "However, I've only ever executed things directly"
  putStrLn "from the libraries through ghci or ghci, like so:"
  putStrLn "$ stack ghci"
  putStrLn "ghci> Challenge.JottingJWTS.main"
  putStrLn "or ghcid which watches files and reloads:"
  putStrLn "$ ghcid -W -r=Challenge.JottingJWTS.main"
