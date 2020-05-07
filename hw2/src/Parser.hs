
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Data.Char
import Data.Time.Clock


data Command = CommandCD FilePath
            | CommandInformation FilePath
            | CommandFindFile String
            | CommandCat String
            | CommandCreateFolder String
            | CommandCreateFile String
            | CommandRemove String
            | CommandWriteFile FilePath String
            | CommandDir
            | CommandLS FilePath
            | CommandExit
            | CommandHelp
            | CommandEmpty 
            deriving (Show)

data Parser s a =
  Parser
    { runParser :: ([s] -> Maybe (a, [s]))
    }

instance Functor (Parser p) where
  fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser p) where
  pure a = Parser $ \inp -> return (a, inp)
  (<*>) p1 p2 =
    Parser $ \inp -> do
      (r1, ost1) <- runParser p1 inp
      (r2, ost2) <- runParser p2 ost1
      return (r1 r2, ost2)

instance Monad (Parser p) where
  return = pure
  (>>=) p1 f =
    Parser $ \inp -> do
      (r, ost) <- runParser p1 inp
      runParser (f r) ost

instance Alternative (Parser p) where
  empty = Parser (const Nothing)
  (<|>) f g = Parser $ \inp -> (runParser f inp) <|> (runParser g inp)

ok :: Parser s ()
ok = Parser $ \inp -> return ((), inp)

eof :: Parser s ()
eof =
  Parser $ \inp ->
    if null inp
      then return ((), inp)
      else Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \case
    [] -> Nothing
    (x:xs) ->
      if p x
        then return (x, xs)
        else Nothing

element :: Eq s => s -> Parser s s
element s = satisfy (== s)

stream :: Eq s => [s] -> Parser s [s]
stream s = traverse element s

digit :: Parser Char Char
digit = satisfy isDigit


commandParser :: Parser Char Command
commandParser = parseCD <|>
                parseInformation <|>
                parseFindFile <|>
                parseCat <|>
                parseCreateFolder <|>
                parseCreateFile <|>
                parseRemove <|>
                parseWriteFile <|>
                parseDir <|>
                parseLS <|>
                parseExit <|>
                parseHelp <|>
                parseEmpty

commandDigit :: Parser Char Char
commandDigit = digit

commandChar :: Parser Char Char
commandChar = satisfy isLetter

commandOtherCharacters :: Parser Char Char
commandOtherCharacters = satisfy (\x -> (x == '.') || (x == '/') || (x == '_') || (x == '-') || (x == '!') || (x == '\''))

parseCD :: Parser Char Command 
parseCD = CommandCD <$> (stream "cd" *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseInformation :: Parser Char Command 
parseInformation = CommandInformation <$> (stream "information" *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseFindFile :: Parser Char Command 
parseFindFile = CommandFindFile <$> (stream "find-file" *> (many $ element ' ') *> parseStringInQuotes) <* (many $ element ' ') <* eof

parseCat :: Parser Char Command 
parseCat = CommandCat <$> (stream "cat" *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseCreateFolder :: Parser Char Command 
parseCreateFolder = CommandCreateFolder <$> (stream "create-folder" *> (many $ element ' ') *> parseStringInQuotes) <* (many $ element ' ') <* eof

parseCreateFile :: Parser Char Command 
parseCreateFile = CommandCreateFile <$> (stream "create-file" *> (many $ element ' ') *> parseStringInQuotes) <* (many $ element ' ') <* eof

parseRemove :: Parser Char Command 
parseRemove = CommandRemove <$> (stream "remove" *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseWriteFile :: Parser Char Command 
parseWriteFile = CommandWriteFile <$> (stream "write-file" *> (many $ element ' ') *> parseString <* (many $ element ' ')) <*> parseStringInQuotes <* (many $ element ' ') <* eof

parseDir :: Parser Char Command 
parseDir = CommandDir <$ stream "dir" <* (many $ element ' ') <* eof

parseLS :: Parser Char Command 
parseLS = CommandLS <$> (stream "ls" *> (many $ element ' ') *> parseString) <* (many $ element ' ') <* eof

parseString :: Parser Char String
parseString = (some (commandDigit <|> commandChar <|> commandOtherCharacters)) <|> (fmap (\x -> "") ok)

parseStringInQuotes :: Parser Char String
parseStringInQuotes = (element '"' *> (some (commandDigit <|> commandChar <|> commandOtherCharacters <|> element ' ')) <* element '"')  <|> (fmap (\x -> "") ok)

parseExit :: Parser Char Command 
parseExit = CommandExit <$ stream "exit" <* (many $ element ' ') <* eof

parseHelp :: Parser Char Command 
parseHelp = CommandHelp <$ stream "help" <* (many $ element ' ') <* eof

parseEmpty :: Parser Char Command
parseEmpty = CommandEmpty <$ eof

-- cd BigHouse
-- write-file HouseHolder.txt "It's VERY SMALL HOUSE"
