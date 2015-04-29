module Language.Dove.Language
  ( Expr
  , Name
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
  , isUnit
  , isBool
  , isInteger
  , isArray
  , isRecord
  , arrayLength
  , (==.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , mod'
  ) where

import Language.Dove.Syntax

let' = Let

forAll :: [Name] -> Expr -> Expr
forAll a b = case a of
  [] -> b
  a : rest -> ForAll a $ forAll rest b

not'        = Unary  Not
infixr 3 &&.
(&&.)       = Binary And
infixr 2 ||.
(||.)       = Binary Or
implies     = Binary Implies
unit        = Literal Unit
true        = Literal $ Bool True
false       = Literal $ Bool False
if'         = Ternary If
arrayLength = Unary ArrayLength
infix 4 ==.
(==.)       = Binary Eq
infix 4 <.
(<.)        = Binary Lt
infix 4 <=.
(<=.)       = Binary Le
infix 4 >.
(>.)        = Binary Gt
infix 4 >=.
(>=.)       = Binary Ge
mod'        = Binary Mod
isUnit      = Unary  IsUnit
isBool      = Unary  IsBool
isInteger   = Unary  IsInteger
isArray     = Unary  IsArray
isRecord    = Unary  IsRecord

