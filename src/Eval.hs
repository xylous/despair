module Eval
    (
    ) where

import Parser

type Program = [Instruction]
type Memory = [(Int, Int)]      -- list of cell indexes and their values
type Pointer = Int

-- Since Brainfuck is holy, I might as well use one-based indexing, equally holy
allocate :: Int -> Memory
allocate n = zip [1..n] $ repeat 0
