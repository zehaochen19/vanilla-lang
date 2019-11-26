module Utils where


freshVarStream :: [String]
freshVarStream = do
  digit  <- nums
  letter <- letters
  return $ letter : show digit
 where
  letters = ['a' .. 'z']
  nums    = [0 ..]
