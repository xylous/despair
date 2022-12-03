module Eval
    ( eval
    , execAll
    ) where

import Parser

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Char (chr)

type Tape = [(Int, Int)]      -- list of cell indexes and their values
type Pointer = Int
data Machine = Machine Tape Pointer deriving (Show, Eq)

-- Allocate `n` entries beginning at index `l`; the last index, therefore, is
-- l+n
allocate :: Int -> Int -> Tape
allocate l n = zip [l..l+n] $ repeat 0

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
    | i == MoveR = return . Just $ Machine tape' (ptr+1)
    | i == MoveL = return . Just $ Machine tape' (ptr-1)
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
        -- Guaranteed to be a Just, if the pointer isn't out of bounds, of
        -- course. Usually, it shouldn't, since we're dynamically allocating
        -- tape as the program runs. But you could definitely pass it, say, a
        -- Machine with a pointer larger than the tape, and it would definitely
        -- crash. That wouldn't be my fault, nor yours, though. It wouldn't be
        -- anyone's fault. Well yeah you could argue it's be my fault, but I
        -- don't take responsibility for this. I'm too tired and too sleepy at
        -- this point to try solving it. Hell, I wasn't even supposed to write
        -- this Brainfuck interpreter today. Did you really expect a fully
        -- fleshed out program that can flawlessly handle each and every error?
        -- You're in for a treat. This is precisely that kind of program... I
        -- mean, as long as the input isn't any bonkers, then sure... it runs
        -- fine I guess?
        current = fromJust $ cellValue m
        -- dynamically allocate more tape if we need it
        -- TODO ASAP: have it based upon the pointer's whereabouts instead of
        -- the absolute length. Maybe create cells as they're used?
        tape' = if l <= ptr then tape ++ allocate l 10 else tape
        l = length tape

-- If the given instruction is a loop, return all instructions within it
loopContents :: Instruction -> [Instruction]
loopContents (Loop xs) = xs
loopContents _ = []

-- Execute loop (list of instructions) only if the current cell's value is
-- greater than zero and, after praying to God that your code is correct, maybe
-- return the final state of the machine
loop :: Machine -> [Instruction] -> IO (Maybe Machine)
loop m is = do
    mm' <- execAll m is
    case mm' of
        Just m' -> do
            let current = cellValue m
            case current of
                Just val    -> if val /= 0 then loop m' is else return . Just $ m
                Nothing     -> return Nothing
        Nothing -> return Nothing

-- Execute all instructions in the instructions chain and return the final state
-- of the machine... Maybe. Just maybe.
execAll :: Machine -> [Instruction] -> IO (Maybe Machine)
execAll m (i:is) = do
    mm' <- eval m i
    case mm' of
        Just m' -> execAll m' is
        Nothing -> return Nothing
execAll m _ = return . Just $ m
