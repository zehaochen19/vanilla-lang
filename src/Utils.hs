module Utils where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Syntax.Expr

freshVarStream :: [Text]
freshVarStream = do
  digit  <- nums
  letter <- letters
  return . T.pack $ '\'' : letter : show digit
 where
  letters = ['a' .. 'z']
  nums    = [0 ..]

natToInt :: Expr -> Integer
natToInt EZero     = 0
natToInt (ESucc n) = 1 + natToInt n
natToInt _         = 0

intToNat :: Integer -> Expr
intToNat n | n <= 0 = EZero
intToNat n          = ESucc . intToNat $ n - 1
