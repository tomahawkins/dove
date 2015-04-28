module Language.Dove.Syntax
  ( Expr (..)
  , UniOp (..)
  , BinOp (..)
  , let'
  , forAll
  , not'
  , (&&.)
  , (||.)
  , implies
  , unit
  , true
  , false
  , if'
  , length'
  , isArray
  , isInt
  , (==.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , mod'
  ) where

import Data.List
import Text.Printf

data Expr
  = Var           String
  | ForAll        String Expr
  | Let           String Expr Expr
  | Record        [(String, Expr)]
  | RecordOverlay Expr Expr       -- overlay first over second
  | RecordProject Expr String
  | Array         [Expr]
  | ArrayAppend   Expr Expr
  | ArrayProject  Expr Expr
  | ArrayUpdate   Expr Expr Expr  -- index newValue oldArray
  | UniOp         UniOp Expr
  | BinOp         BinOp Expr Expr
  | If            Expr Expr Expr
  | Unit
  | Bool          Bool
  | Integer       Integer
  | Comment       String Expr
  deriving Eq

data UniOp
  = Not
  | Length
  | Negate
  | Abs
  | Signum
  | IsArray
  | IsInt
  deriving Eq

data BinOp
  = And
  | Or
  | Implies
  | Eq
  | Lt
  | Le
  | Gt
  | Ge
  | Add
  | Sub
  | Mul
  | Mod
  deriving Eq

instance Show Expr where
  show a = case a of
    Var           a     -> a
    ForAll        a b   -> printf "forall %s in\n%s" a (show b)
    Record        a     -> printf "{%s}" $ intercalate ", " [ a ++ " = " ++ show b | (a, b) <- a ]
    RecordOverlay a b   -> printf "(overlay %s %s)" (show a) (show b)
    RecordProject a b   -> printf "%s.%s" (show a) b
    Array         a     -> printf "[%s]" $ intercalate ", " $ map show a
    ArrayAppend   a b   -> printf "(%s ++ %s)" (show a) (show b)
    ArrayUpdate   a b c -> printf "(update %s %s %s)" (show a) (show b) (show c)
    ArrayProject  a b   -> printf "%s[%s]"  (show a) (show b)
    Let           a b c -> printf "let %s = %s in\n%s" a (show b) (show c)
    UniOp         a b   -> printf "(%s %s)" (show a) (show b)
    BinOp         a b c -> printf "(%s %s %s)" (show b) (show a) (show c)
    If            a b c -> printf "(if %s then %s else %s)" (show a) (show b) (show c)
    Unit                -> "()"
    Bool          a     -> if a then "true" else "false"
    Integer       a     -> show a
    Comment       a b   -> "-- " ++ a ++ "\n" ++ show b

instance Show UniOp where
  show a = case a of
    Not     -> "not"
    Length  -> "arrayLength"
    Negate  -> "intNegate"
    Abs     -> "intAbs"
    Signum  -> "intSignum"
    IsArray -> "isArray"
    IsInt   -> "isInt"

instance Show BinOp where
  show a = case a of
    And     -> "and"
    Or      -> "or"
    Implies -> "implies"
    Eq      -> "eq"
    Lt      -> "lt"
    Le      -> "le"
    Gt      -> "gt"
    Ge      -> "ge"
    Add     -> "+"
    Sub     -> "-"
    Mul     -> "*"
    Mod     -> "mod"

instance Num Expr where
  (+)    = BinOp Add
  (-)    = BinOp Sub
  (*)    = BinOp Mul
  negate = UniOp Negate
  abs    = UniOp Abs
  signum = UniOp Signum
  fromInteger = Integer

let' = Let

forAll :: [String] -> Expr -> Expr
forAll a b = case a of
  [] -> b
  a : rest -> ForAll a $ forAll rest b

not' = UniOp Not
(&&.) = BinOp And
(||.) = BinOp Or
implies = BinOp Implies
unit  = Unit
true  = Bool True
false = Bool False
if' = If
length' = UniOp Length
(==.) = BinOp Eq
(<.)  = BinOp Lt
(<=.) = BinOp Le
(>.)  = BinOp Gt
(>=.) = BinOp Ge
mod' = BinOp Mod
isArray = UniOp IsArray
isInt = UniOp IsInt

