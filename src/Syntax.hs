module Syntax where

type Name = String

data ConstBind = ConstBind Name Expr
  deriving (Eq, Ord, Show)

data ProcBind = ProcBind Name Prog
  deriving (Eq, Ord, Show)

data Prog = Prog [ConstBind] [Name] [ProcBind] Stmt
  deriving (Eq, Ord, Show)

data Stmt
  = AssignStmt Name Expr
  | IfStmt Cond Stmt
  | WhileStmt Cond Stmt
  | CallStmt Name
  | ReadStmt [Name]
  | WriteStmt [Expr]
  | CompStmt [Stmt]
  | EmptyStmt
  deriving (Eq, Ord, Show)

data ArithOp = AddOp | SubOp | MulOp | DivOp
  deriving (Eq, Ord, Show)

data RelatOp = EqOp | NeOp | LtOp | LeOp | GtOp | GeOp
  deriving (Eq, Ord, Show)

data Cond
  = RelatCond RelatOp Expr Expr
  | OddCond Expr
  deriving (Eq, Ord, Show)

data Expr
  = ExprInt Int
  | ExprIdent Name
  | ExprOp ArithOp Expr Expr
  | ExprNeg Expr
  deriving (Eq, Ord, Show)

