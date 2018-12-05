module Parser where

import Lexer
import Syntax
import ParserCombinator
import Control.Applicative

prog :: Parser Prog
prog = do p <- subprog
          symbol "."
          return p

subprog :: Parser Prog
subprog = do cs <- constDecl
             vs <- varDecl
             fs <- many procDecl
             s  <- stmt
             return $ Prog cs vs fs s

constDecl :: Parser [ConstBind]
constDecl = do reserved "const"
               xs <- constBind `sepBy` (symbol ",")
               symbol ";"
               return xs
           <|> return []

constBind :: Parser ConstBind
constBind = do x <- identifier
               symbol "="
               y <- expr
               return $ ConstBind x y

varDecl :: Parser [Name]
varDecl = do reserved "var"
             xs <- identifier `sepBy` (symbol ",")
             symbol ";"
             return xs
         <|> return []

procDecl :: Parser ProcBind
procDecl = do reserved "procedure"
              x <- identifier
              symbol ";"
              p <- subprog
              symbol ";"
              return $ ProcBind x p

stmt :: Parser Stmt
stmt = assignStmt
   <|> ifStmt
   <|> whileStmt
   <|> readStmt
   <|> writeStmt
   <|> callStmt
   <|> compStmt
   <|> return EmptyStmt

assignStmt :: Parser Stmt
assignStmt = do x <- identifier
                symbol ":="
                y <- expr
                return $ AssignStmt x y

ifStmt :: Parser Stmt
ifStmt = do reserved "if"
            c <- cond
            reserved "then"
            s <- stmt
            return $ IfStmt c s

whileStmt :: Parser Stmt
whileStmt = do reserved "while"
               c <- cond
               reserved "do"
               s <- stmt
               return $ WhileStmt c s

readStmt :: Parser Stmt
readStmt = do reserved "read"
              symbol "("
              xs <- identifier `sepBy` (symbol ",")
              symbol ")"
              return $ ReadStmt xs

writeStmt :: Parser Stmt
writeStmt = do reserved "write"
               symbol "("
               xs <- expr `sepBy` (symbol ",")
               symbol ")"
               return $ WriteStmt xs

callStmt :: Parser Stmt
callStmt = do reserved "call"
              x <- identifier
              return $ CallStmt x

compStmt :: Parser Stmt
compStmt = do reserved "begin"
              sts <- stmt `sepBy` (symbol ";")
              reserved "end"
              return $ CompStmt sts

expr :: Parser Expr
expr = do t <- term
          do symbol "+"; e <- expr; return $ ExprOp AddOp t e
           <|> do symbol "-"; e <- expr; return $ ExprOp SubOp t e
           <|> return t
   <|> do symbol "-"; e <- expr; return $ ExprNeg e

term :: Parser Expr
term = do f <- factor
          do symbol "*"; t <- term; return $ ExprOp MulOp f t
           <|> do symbol "/"; t <- term; return $ ExprOp DivOp f t
           <|> return f

factor :: Parser Expr
factor = do symbol "("; e <- expr; symbol ")"; return e
     <|> do x <- integer; return $ ExprInt x
     <|> do a <- identifier; return $ ExprIdent a

relatOp :: Parser RelatOp
relatOp = do symbol "=";  return EqOp
      <|> do symbol "#";  return NeOp
      <|> do symbol "<";  return LtOp
      <|> do symbol "<="; return LeOp
      <|> do symbol ">";  return GtOp
      <|> do symbol ">="; return GeOp

cond :: Parser Cond
cond = do reserved "odd"; e <- expr; return $ OddCond e
   <|> do e1 <- expr; op <- relatOp; e2 <- expr; return $ RelatCond op e1 e2
