module Lexer where

import Data.Char
import ParserCombinator
import Control.Applicative

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

item :: Parser Char
item = P (\inp -> case inp of
             [] -> []
             (x:xs) -> [(x, xs)])

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do a <- p
                 as <- many (do sep; p)
                 return (a:as)

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char = sat . (==)

lowerChar :: Char -> Parser Char
lowerChar ch = sat (\x -> toLower(x) == toLower(ch))

ident :: Parser String
ident = do x <- letter
           xs <- many alphanum
           return (x:xs)

rsv :: String -> Parser ()
rsv [] = return ()
rsv (x:xs) = do lowerChar x
                rsv xs
                return ()

sym :: String -> Parser ()
sym [] = return ()
sym (x:xs) = do char x
                sym xs
                return ()

int :: Parser Int
int = do xs <- some digit
         return $ read xs

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

integer :: Parser Int
integer = token int

reserved :: String -> Parser ()
reserved = token . rsv

symbol :: String -> Parser ()
symbol = token . sym
