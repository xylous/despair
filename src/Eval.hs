module Eval
    (
    ) where

import Parser

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Char (chr)

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

-- TODO: types look too ugly. Maybe try fixing them later?
eval :: Machine -> Instruction -> IO (Maybe Machine)
eval m i
    | i == MoveR = return . Just $ Machine tape (ptr+1)
    | i == MoveL = return . Just $ Machine tape (ptr-1)
    | i == Increment = return . Just $ changeState (+1) m
    | i == Decrement = return . Just $ changeState (subtract 1) m
    | (not . null) iLoop = loop m iLoop
    | i == Output = do
        print $ chr current
        return (Just m)
    | i == Input = do
        line <- getLine
        let newval = read line :: Int
        let m' = changeState (+ (newval - current)) m
        return . Just $ m'
    | otherwise = undefined -- should never happen
      where
        (Machine tape ptr) = m
        iLoop = loopContents i
        current = fromJust $ cellValue m

-- If the given instruction is a loop, return all instructions within it
loopContents :: Instruction -> [Instruction]
loopContents (Loop xs) = xs
loopContents _ = []

-- Execute loop only if the current cell's value is greater than zero and, pray
-- to God your code is correct, maybe return the final state of the machine
loop :: Machine -> [Instruction] -> IO (Maybe Machine)
loop m is = do
    m' <- execAll m is
    let current = fromJust $ cellValue m
    if current /= 0 then loop (fromJust m') is else return . Just $ m

-- Execute all instructions in the instructions chain and return the final state
-- of the machine... Maybe. Just maybe.
execAll :: Machine -> [Instruction] -> IO (Maybe Machine)
execAll m (i:is) = do
    m' <- eval m i
    execAll (fromJust m') is
execAll m _ = return . Just $ m