module Eval
    (
    ) where

import Parser

import Data.List (find)

type Program = [Instruction]
type Tape = [(Int, Int)]      -- list of cell indexes and their values
type Pointer = Int
data Machine = Machine Tape Pointer deriving (Show, Eq)

-- Since Brainfuck is holy, I might as well use one-based indexing, equally holy
allocate :: Int -> Tape
allocate n = zip [1..n] $ repeat 0

-- Apply the given function f only on the cell that the pointer is indicating,
-- changing its value; don't alter any other cells
changeState :: (Int -> Int) -> Machine -> Machine
changeState f (Machine tape ptr) = Machine tape' ptr
  where
    tape' = map (\(k,v) -> (k, if k == ptr then f v else v)) tape

-- Extract the value of the cell at the pointer index, or Nothing if there's
-- no such index
cellValue :: Machine -> Maybe Int
cellValue (Machine tape ptr) = do
    (_,v) <- find (\(k,_) -> k == ptr) tape
    Just v

eval :: Machine -> Instruction -> Machine
eval m i
    | i == MoveR = Machine tape (ptr+1)
    | i == MoveL = Machine tape (ptr-1)
    | i == Increment = changeState (+1) m
    | i == Decrement = changeState (subtract 1) m
    | otherwise = undefined -- should never happen
      where
        (Machine tape ptr) = m
