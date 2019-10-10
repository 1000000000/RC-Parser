module Parser where

import Control.Applicative
import Control.Arrow
import Control.Monad

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser p) = Parser $
    \str ->
      fmap (first f) (p str)

instance Applicative Parser where
  pure val = Parser $ \str -> [(val, str)]
  (Parser fp) <*> (Parser vp) = Parser $
    \str ->
      let
        valResults = vp str
        applyInTuple val (fn, rem) = (fn val, rem)
      in
        concatMap (\(val, remainder) -> fmap (applyInTuple val) $ fp remainder) valResults

instance Monad Parser where
  p >>= mf = Parser $
    \str ->
      let
        results = runParser (fmap (runParser . mf) p) str
      in
        concatMap (uncurry ($)) results

instance Alternative Parser where
  empty = Parser $ const []
  (Parser p1) <|> (Parser p2) = Parser $
    \str ->
      if not $ null $ p1 str
        then p1 str
        else p2 str

fullMatch :: Parser a -> Parser a
fullMatch (Parser p) = Parser $
  \str ->
    filter (null . snd) $ p str

matchChar :: (Char -> Bool) -> Parser Char
matchChar pred = Parser $
  \str ->
    case str of
      []     -> []
      (c:cs) ->
        if pred c
          then [(c, cs)]
          else []

string :: String -> Parser String
string = fmap reverse . mapM (matchChar . (==)) . reverse

optional :: a -> Parser a -> Parser a
optional def parser = parser <|> pure def

