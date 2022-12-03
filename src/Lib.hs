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
                 deriving (Show, Eq)
type Program = [Instruction]
type Memory = [(Int, Int)]      -- list of cell indexes and their values
type Pointer = Int

toInstruction :: Char -> Instruction
toInstruction ch
    | ch == '>' = MoveR
    | ch == '<' = MoveL
    | ch == '+' = Increment
    | ch == '-' = Decrement
    | ch == '.' = Output
    | ch == ',' = Input
    | ch == '[' = JumpFw
    | ch == ']' = JumpBw
    -- Don't worry, this should never happen
    | otherwise = undefined

-- Since Brainfuck is holy, I might as well use one-based indexing, equally holy
allocate :: Int -> Memory
allocate n = zip [1..n] $ repeat 0
