module Syntax where
import qualified Text.Parsec.Token as T

type Identifier = String

data Atom
  = Character Char
  | Number Int
  | True
  | False
    deriving (Eq, Show)

data Name
  = Identifier Identifier
  | Operator String
    deriving (Eq, Show, Ord)

data Sequence
  = Sequence [Expr]
  | Str String
    deriving (Eq)

instance Show Sequence where
  show (Sequence exprs) = "<Sequence: " ++ show exprs ++" >"
  show (Str str) = "<String: " ++ str ++ " >"

data Pattern
  = Elementary Name (Maybe PatExpr)
  | PatConstruction PatList
    deriving (Eq, Show)

data PatExpr
  = Pat Pattern
  | Expr Expr
    deriving (Eq, Show)

data PatList
  = PatList [PatExpr] Pattern [PatExpr]
    deriving (Eq, Show)

data Cond
  = CondExpr Expr Expr
  | CondPat  Pattern Expr
  | CondAlternative Expr Expr
    deriving (Eq, Show)

data Expr
  = Atom Atom
  | Name Name
  | Seq Sequence
  | Application Expr Expr
  | Primed Expr -- Expr'
  | Cond Cond
  | Constant Expr
  | Constr [Expr]
  | PredicateConstr [Expr]
--  | Infix Expr Expr Expr -- not implemented since parsing could get weird?
  | Where Expr Env
    deriving (Eq, Show)

data Env
  = Defns [Defn]
  | Export [Name] Env
  | Hide [Name] Env
  | Lib String
  | PF
  | Uses Env Env
  | EWhere Env Env
  | Union Env Env
  | Defn Defn
  | EExpr Expr
    deriving (Eq, Show)

data Top = TEnv Env | TDefn Defn | TExpr Expr
         deriving (Eq, Show)

data Defn
  = Def Name (Maybe PatExpr) Expr
  | NRDef Name (Maybe PatExpr) Expr
  | Type Identifier Pattern
    deriving (Eq, Show)
