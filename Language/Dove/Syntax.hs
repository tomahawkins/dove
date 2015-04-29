module Language.Dove.Syntax
  ( Expr    (..)
  , Literal (..)
  , Unary   (..)
  , Binary  (..)
  , Ternary (..)
  , Name
  ) where

import Data.List
import Text.Printf

type Name = String

data Expr
  = Var           Name
  | Literal       Literal
  | ForAll        Name Expr
  | Let           Name Expr Expr
  | Record        [(Name, Expr)]
  | RecordProject Expr Name
  | Array         [Expr]
  | Unary         Unary   Expr
  | Binary        Binary  Expr Expr
  | Ternary       Ternary Expr Expr Expr
  | Comment       String Expr
  deriving Eq

data Literal = Unit | Bool Bool | Integer Integer deriving Eq

data Unary
  = IsUnit
  | IsBool
  | IsInteger
  | IsArray
  | IsRecord
  | ArrayLength
  | Not
  | Negate
  | Abs
  | Signum
  deriving Eq

data Binary
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
  | RecordOverlay  -- ^ Overlay first over second.
  | ArrayAppend
  | ArrayProject   -- ^ Array, index.
  deriving Eq

data Ternary
  = If
  | ArrayUpdate  -- ^ Index, newValue, oldArray.
  deriving Eq

instance Show Expr where
  show a = case a of
    Var           a     -> a
    Literal       Unit  -> "()"
    Literal       (Bool True)  -> "true"
    Literal       (Bool False) -> "false"
    Literal       (Integer a)  -> show a
    ForAll        a b   -> printf "forall %s in\n%s" a (show b)
    Let           a b c -> printf "let %s = %s in\n%s" a (show b) (show c)
    Record        a     -> printf "{%s}" $ intercalate ", " [ a ++ " = " ++ show b | (a, b) <- a ]
    RecordProject a b   -> printf "%s.%s" (show a) b
    Array         a     -> printf "[%s]" $ intercalate ", " $ map show a
    Unary         a b   -> printf "(%s %s)" (show a) (show b)
    Binary        a b c -> printf "(%s %s %s)" (show a) (show b) (show c)
    Ternary       If          a b c -> printf "(if %s then %s else %s)" (show a) (show b) (show c)
    Ternary       ArrayUpdate a b c -> printf "(arrayUpdate %s %s %s)"  (show a) (show b) (show c)
    Comment       a b   -> "-- " ++ a ++ "\n" ++ show b

instance Show Unary where
  show a = case a of
    IsUnit      -> "isUnit"
    IsBool      -> "isBool"
    IsInteger   -> "isInteger"
    IsArray     -> "isArray"
    IsRecord    -> "isRecord"
    ArrayLength -> "arrayLength"
    Not         -> "not"
    Negate      -> "negate"
    Abs         -> "abs"
    Signum      -> "signum"

instance Show Binary where
  show a = case a of
    And           -> "and"
    Or            -> "or"
    Implies       -> "implies"
    Eq            -> "eq"
    Lt            -> "lt"
    Le            -> "le"
    Gt            -> "gt"
    Ge            -> "ge"
    Add           -> "+"
    Sub           -> "-"
    Mul           -> "*"
    Mod           -> "mod"
    RecordOverlay -> "recordOverlay"
    ArrayAppend   -> "++"
    ArrayProject  -> "!"

instance Num Expr where
  (+)         = Binary Add
  (-)         = Binary Sub
  (*)         = Binary Mul
  negate      = Unary Negate
  abs         = Unary Abs
  signum      = Unary Signum
  fromInteger = Literal . Integer

