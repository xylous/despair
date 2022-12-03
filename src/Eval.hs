module Eval
    (
    ) where

import Parser

type Program = [Instruction]
type Tape = [(Int, Int)]      -- list of cell indexes and their values
type Pointer = Int
data Machine = Machine Tape Pointer

-- Since Brainfuck is holy, I might as well use one-based indexing, equally holy
allocate :: Int -> Tape
allocate n = zip [1..n] $ repeat 0

eval :: Machine -> Instruction -> Machine
eval (Machine tape ptr) i
    | i == MoveR = Machine tape (ptr + 1)
    | i == MoveL = Machine tape (ptr + 1)
    | otherwise = undefined -- should never happen
