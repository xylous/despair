module Eval
    ( eval
    , execAll
    , evalFile
    , evalFileO
    ) where

import Parser

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Char (chr)
import Control.Monad (foldM)

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
eval :: Machine -> Instruction -> IO Machine
eval m i = case i of
            MoveR -> return $ Machine (tape' (ptr + 1)) (ptr + 1)
            MoveL -> return $ Machine (tape' (ptr - 1)) (ptr - 1)
            Increment -> return $ changeState (+1) m
            Decrement -> return $ changeState (subtract 1) m
            Output -> do
                putChar $ chr cv
                return m
            Input -> do
                line <- getLine
                let newval = read line :: Int
                return $ changeState (+ (newval - cv)) m
            Loop xs -> loop m xs
            _ -> return m
      where
        (Machine tape ptr) = m
        cv = fromMaybe 0 (cellValue m)
        -- Dynamically allocate more tape if we need it - practically, create
        -- cells at the new pointer's location, if it's needed
        tape' = moreTape tape

-- Execute loop (list of instructions) only if the current cell's value is
-- greater than zero and, after praying to God that your code is correct, maybe
-- return the final state of the machine
loop :: Machine -> [Instruction] -> IO Machine
loop m is = do
    m' <- execAll m is
    let cv = fromMaybe 0 (cellValue m)
    if cv /= 0 then loop m' is else return m

-- Execute all instructions in the instructions chain and return the final state
-- of the machine... Maybe. Just maybe.
execAll :: Machine -> [Instruction] -> IO Machine
execAll = foldM eval

-- Evaluate the program in the given file and return the resulting machine
evalFile :: FilePath -> IO Machine
evalFile path = do
    is <- parseFile path
    execAll (Machine [(0,0)] 0) is

-- Turn a list of instructions into a list of simple instructions; no less, no
-- more
simplify :: [Instruction] -> [SimpleIns]
simplify [] = []
simplify is =
    let result = case head is of
            Loop xs -> [SimpleLoop (simplify xs)]
            Increment -> ifNotNull AddCell lenConsumedOps
            Decrement -> ifNotNull AddCell (negate lenConsumedOps)
            MoveR -> ifNotNull AddPtr lenConsumedOps
            MoveL -> ifNotNull AddPtr (negate lenConsumedOps)
            Output -> [SOutput lenConsumedOps]
            Input -> [SInput lenConsumedOps]
            _ -> []
        in result ++ simplify (drop lenConsumedOps is)
  where
    lenConsumedOps = length matchingOps
    matchingOps = takeFirstMatching is
    ifNotNull :: (Int -> SimpleIns) -> Int -> [SimpleIns]
    ifNotNull i n = [i n | n /= 0]

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

hasIndex :: Tape -> Int -> Bool
tape `hasIndex` n = case cellValue (Machine tape n) of
                        Just _ -> True
                        Nothing -> False

moreTape :: Tape -> Int -> Tape
moreTape tape n = if tape `hasIndex` n then tape else tape ++ [(n, 0)]

execO :: Machine -> SimpleIns -> IO Machine
execO m s = case s of
    AddCell n -> return $ changeState (+n) m
    AddPtr n -> return $ Machine (moreTape tape (ptr + n)) (ptr + n)
    SimpleLoop xs -> loopO m xs
    SOutput n -> do
        putStr $ replicate n (chr cv)
        return m
    SInput _ -> do
        line <- getLine
        let newval = read line :: Int
        let m' = changeState (+ (newval - cv)) m
        return m'
  where
    (Machine tape ptr) = m
    cv = fromMaybe 0 (cellValue m)

execAllO :: Machine -> [SimpleIns] -> IO Machine
execAllO = foldM execO

loopO :: Machine -> [SimpleIns] -> IO Machine
loopO m sis = do
    m' <- execAllO m sis
    if cv /= 0 then loopO m' sis else return m
  where
    cv = fromMaybe 0 (cellValue m)

evalFileO :: FilePath -> IO Machine
evalFileO path = do
    inp <- parseFile path
    let simplified = simplify inp
    execAllO (Machine [(0,0)] 0) simplified
