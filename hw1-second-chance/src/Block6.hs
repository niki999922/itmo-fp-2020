{-# LANGUAGE LambdaCase #-}

module Block6
  ( Parser(..)
  , ok
  , eof
  , satisfy
  , element
  , stream
  , pspParser
  , numberParser
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Data.Char

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

pspParser :: Parser Char ()
pspParser = psp *> eof
  where
    psp = (element '(' *> psp *> element ')' *> psp) <|> ok

numberParser :: Parser Char String
numberParser =
  (\x y -> x : y) <$> (element '+' <|> element '-' <|> (fmap (\_ -> '+') ok)) <*>
  (some digit)
