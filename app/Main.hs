module Main (main) where

import Eval (evalFileO)

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    -- ignore the final state of the machine
    _ <- evalFileO (head args)
    return ()
