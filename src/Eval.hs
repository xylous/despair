{-# LANGUAGE LambdaCase #-}

module Eval
    ( eval
    , execAll
    , evalFile
    ) where

import Parser

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Char (chr)

type Tape = [(Int, Int)]      -- list of cell indexes and their values
type Pointer = Int
data Machine = Machine Tape Pointer deriving (Show, Eq)

-- Basically the same as an Instruction but now every operation (except the
-- loop) is repeated some number of times
-- It's used by the optimiser to do things quicker
data SimpleIns = AddCell Int
               | AddPtr Int
               | SInput Int
               | SOutput Int
               | SimpleLoop [SimpleIns]
               deriving (Show, Eq)

-- Allocate `n` entries beginning at index `l`; the last index, therefore, is
-- l+n
allocate :: Int -> Int -> Tape
allocate l n = zip [l..l+n-1] $ repeat 0

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
    | i == MoveR = return . Just $ Machine tape'R ptr'R
    | i == MoveL = return . Just $ Machine tape'L ptr'L
    | i == Increment = return . Just $ changeState (+1) m
    | i == Decrement = return . Just $ changeState (subtract 1) m
    | i == Output = do
        putChar $ chr current
        return (Just m)
    | i == Input = do
        line <- getLine
        let newval = read line :: Int
        let m' = changeState (+ (newval - current)) m
        return . Just $ m'
    | (not . null) iLoop = loop m iLoop
    | otherwise = undefined -- should never happen
      where
        (Machine tape ptr) = m
        iLoop = loopContents i
        current = fromMaybe 0 (cellValue m)
        -- Dynamically allocate more tape if we need it - practically, create
        -- cells at the new pointer's location, if it's needed
        tape'R = moreTape tape ptr'R
        tape'L = moreTape tape ptr'L
        ptr'R = ptr + 1
        ptr'L = ptr - 1

-- If the pointer is pointing to a nonexistent cell, return a tape with that
-- cell allocated/created
moreTape :: Tape -> Pointer -> Tape
moreTape tape ptr = case cellValue (Machine tape ptr) of
                        Just _  -> tape
                        Nothing -> tape ++ allocate ptr 1

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

-- Evaluate the program in the given file and return the resulting machine
evalFile :: FilePath -> IO (Maybe Machine)
evalFile path = do
    is <- parseFile path
    let m = Machine (allocate 0 1) 0
    execAll m is

-- Turn a list of instructions into a list of simple instructions; no less, no
-- more
simplify :: [Instruction] -> [SimpleIns]
simplify [] = []
simplify is =
    let result = case head is of
            Loop xs -> [SimpleLoop (simplify xs)]
            Increment -> ifNotNull AddCell deltaN
            Decrement -> ifNotNull AddCell (negate deltaN)
            MoveR -> ifNotNull AddPtr deltaN
            MoveL -> ifNotNull AddPtr (negate deltaN)
            Output -> [SOutput lenConsumedOps]
            Input -> [SInput lenConsumedOps]
            _ -> []
        in result ++ simplify (drop lenConsumedOps is)
  where
    lenConsumedOps = length matchingOps
    matchingOps = takeFirstMatching is
    ifNotNull :: (Int -> SimpleIns) -> Int -> [SimpleIns]
    ifNotNull i n = [i n | n /= 0]
    deltaN = sum $ map (\x -> if x == Decrement || x == MoveL then negate 1 else 1) matchingOps

-- Two instructions are "matching" if they have the same role, i.e they refer to
-- the same thing. `+` and `+` are matching because they both increment the
-- a cell's value, but `+` and `-` aer also matching; `+` and `<` aren't
-- matching, but `<` and `>` are, and so on and so forth
-- Note that all identical operations are considered matching

-- This function takes the first "matching" instructions until it finds one that
-- isn't matching
takeFirstMatching :: [Instruction] -> [Instruction]
takeFirstMatching [] = []
takeFirstMatching is = takeWhile f is
    where
        hi = head is
        f :: Instruction -> Bool
        f x
            | x == hi = True
            | x == Increment && hi == Decrement = True
            | x == Decrement && hi == Increment = True
            | x == MoveR && hi == MoveL = True
            | x == MoveL && hi == MoveR = True
            | otherwise = False
