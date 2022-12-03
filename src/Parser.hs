module Parser
    ( Instruction
    , parse
    ) where

import Control.Applicative

data Instruction = MoveR
                 | MoveL
                 | Increment
                 | Decrement
                 | Output
                 | Input
                 | Loop [Instruction]
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

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (c:rest)
        | c == x = Just (rest, c)
        | otherwise = Nothing
    f _ = Nothing

parse :: String -> Maybe [Instruction]
parse [] = Just []
parse str = do
    (rest, x) <- runParser instruction str
    fmap (x :) (parse rest)

instruction :: Parser Instruction
instruction = moveR <|> moveL <|> increment <|> decrement <|> output <|> input <|> loop

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
