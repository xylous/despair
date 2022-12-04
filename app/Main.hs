module Main (main) where

import Eval (evalFile)

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    _ <- evalFile (head args)
    return ()
