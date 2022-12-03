module Lib
    (
    ) where

data Instruction = MoveR
                 | MoveL
                 | Increment
                 | Decrement
                 | Output
                 | Input
                 | JumpFw
                 | JumpBw
type Program = [Instruction]
type Memory = [(Int, Int)]      -- list of cell indexes and their values
type Pointer = Int
