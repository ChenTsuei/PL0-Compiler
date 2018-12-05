module ParserCombinator where

import Control.Applicative

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> (String -> [(a, String)])
parse (P p) = p

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of
                   [] -> []
                   [(v, out)] -> [(g v, out)])          

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px = P (\inp -> case parse pg inp of
                    [] -> []
                    [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
                  [] -> []
                  [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\inp -> [])
  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v, out)] -> [(v, out)])
