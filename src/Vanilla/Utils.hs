{-# LANGUAGE OverloadedStrings #-}

module Vanilla.Utils where

import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Vanilla.Syntax.Cons
import Vanilla.Syntax.Expr

freshVarStream :: [Text]
freshVarStream = do
  digit <- nums
  letter <- letters
  return . T.pack $ '\'' : letter : show digit
  where
    letters = ['a' .. 'z']
    nums = [0 ..]

natToInt :: Expr -> Integer
natToInt EZero = 0
natToInt (ESucc n) = 1 + natToInt n
natToInt _ = 0

intToNat :: Integer -> Expr
intToNat n | n <= 0 = EZero
intToNat n = ESucc . intToNat $ n - 1

natToInt' :: Expr -> Integer
natToInt' (ECons (MkConsVar "Zero") mempty) = 0
natToInt' (ECons (MkConsVar "Succ") (S.Empty S.:|> n)) = 1 + natToInt' n
natToInt' _ = 0

intToNat' :: Integer -> Expr
intToNat' n | n <= 0 = cons "Zero"
intToNat' n = cons' "Succ" [intToNat' $ n -1]
