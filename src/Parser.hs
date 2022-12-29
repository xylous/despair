module Parser
    ( Instruction (..)
    , parse
    , parseFile
    ) where

import Control.Applicative
import Data.Char (isSpace)

data Instruction = MoveR
                 | MoveL
                 | Increment
                 | Decrement
                 | Output
                 | Input
                 | Loop [Instruction]
                 | Comment String
                 deriving (Show, Eq)

-- TODO: implement proper error reporting
newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \str -> do
        (rest, x) <- p str
        Just (rest, f x)

instance Applicative Parser where
    pure x = Parser $ \str -> Just (str, x)
    (Parser p1) <*> (Parser p2) = Parser $ \str -> do
        (rest, f) <- p1 str
        (rest', x) <- p2 rest
        Just (rest', f x)

instance Alternative Parser where
    -- An empty parser is one that straight up fails
    empty = Parser $ const Nothing
    -- since the result of a parser is wrapped in a Maybe, we can rely on its
    -- implementation of Alternative for the most part
    (Parser p1) <|> (Parser p2) = Parser $ \str -> p1 str <|> p2 str

parse :: String -> Maybe [Instruction]
parse [] = Just []
parse str = do
    (rest, x) <- runParser instruction str
    fmap (x :) (parse rest)

-- NOTE: all comments are stripped
parseFile :: String -> IO [Instruction]
parseFile path = do
    contents <- readFile path
    case parse contents of
        Just is -> return $ stripComments is
        _ -> return []

instruction :: Parser Instruction
instruction = moveR <|> moveL <|> increment <|> decrement <|> output <|> input <|> loop <|> comment

moveR :: Parser Instruction
moveR = MoveR <$ charP '>'

moveL :: Parser Instruction
moveL = MoveL <$ charP '<'

increment :: Parser Instruction
increment = Increment <$ charP '+'

decrement :: Parser Instruction
decrement = Decrement <$ charP '-'

output :: Parser Instruction
output = Output <$ charP '.'

input :: Parser Instruction
input = Input <$ charP ','

loop :: Parser Instruction
loop = Loop <$> (charP '[' *> many instruction <* charP ']')

-- comments should be disregarded, but in order to disregard them we have to
-- know what they are
comment :: Parser Instruction
comment = Comment <$>
            (
                -- as an extension, a comment starting with `;` ends on newline
                -- this is an innocent extension that does no harm. it doesn't
                -- bite
                charP ';' *> spanP (/='\n')
                -- somehow not parsing empty whitespace separately breaks
                -- things. If It Works It Works:TM:
                <|> spanP isSpace
                -- as per the "standard" specification, all but the eight
                -- command characters are treated as comments
                <|> spanP (not . isBFChar)
            )

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (c:rest)
        | c == x = Just (rest, c)
        | otherwise = Nothing
    f _ = Nothing

-- take f and feed it to span over the parser's input; if the result is an empty
-- list, meaning the predicate failed, then return Nothing, otehrwise return the
-- rest of the string and the parsed token
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \x ->
    let (token, rest) = span f x
     in if null token then Nothing else Just (rest, token)

isBFChar :: Char -> Bool
isBFChar x = x `elem` "+-><,.[]"

stripComments :: [Instruction] -> [Instruction]
stripComments [] = []
stripComments (i:is) = case i of
                        Comment _ -> stripComments is
                        Loop xs -> Loop (stripComments xs) : stripComments is
                        _ -> i : stripComments is
