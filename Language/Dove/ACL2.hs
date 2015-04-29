-- | Interface to ACL2.
module Language.Dove.ACL2
  ( acl2
  , check
  , check'
  ) where

import qualified Language.ACL2 as A

import Language.Dove.Syntax

check :: Expr -> IO Bool
check a = check' a >>= return . fst

check' :: Expr -> IO (Bool, String)
check' a = A.check' [A.thm $ acl2 a]

acl2 :: Expr -> A.Expr
acl2 a = case a of
  Var a -> A.var a
  ForAll _ a -> expr a  -- XXX Assumes variable does not shadow other variables.
  Let a b c -> A.let' [(a, expr b)] $ expr c
  If a b c -> A.if' (expr a) (expr b) (expr c)
  Unit -> A.nil
  Bool a -> if a then A.t else A.nil
  Integer a -> A.lit $ show a
  Comment _ a -> expr a
  Array a -> A.list $ map expr a
  ArrayAppend a b -> A.append (expr a) (expr b)
  ArrayProject a b -> A.nth (expr b) (expr a)
  ArrayUpdate a b c -> A.updateNth (expr a) (expr b) (expr c)
  Record a -> A.list [ A.cons (A.string a) (expr b) | (a, b) <- a ]
  RecordOverlay a b  -> A.append (expr a) (expr b)
  RecordProject a b -> A.cdr $ A.assoc (A.string b) (expr a)
  Unary op a -> case op of
    IsUnit    -> A.equal (A.nil) (expr a)
    IsBool    -> A.or' (A.equal A.nil (expr a)) (A.equal A.t (expr a))
    IsInteger -> A.integerp $ expr a
    IsArray   -> A.consp $ expr a   -- Arrays can't be zero length.  A.or' (A.equal A.nil $ expr a) (A.consp $ expr a)
    IsRecord  -> undefined          -- XXX How to differentiate between arrays and records?  Perhaps values need to be boxed.
    Not     -> A.not' $ expr a
    Length  -> A.len  $ expr a
    Negate  -> 0 - (expr a)
    Abs     -> A.if' (expr a A.>=. 0) (expr a) (0 - (expr a))
    Signum  -> A.if' (expr a A.>. 0) 1 $ A.if' (expr a A.<. 0) (-1) 0
  BinOp op a b -> op' (expr a) (expr b)
    where
    op' = case op of
      And     -> A.and'
      Or      -> A.or'
      Implies -> A.implies
      Eq      -> A.equal
      Lt      -> (A.<.)
      Le      -> (A.<=.)
      Gt      -> (A.>.)
      Ge      -> (A.>=.)
      Add     -> (+)
      Sub     -> (-)
      Mul     -> (*)
      Mod     -> A.mod'
  where
  expr = acl2

