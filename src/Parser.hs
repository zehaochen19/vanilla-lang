{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Functor (($>))
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Syntax.Expr
import Syntax.Type
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

dotP :: Parser Text
dotP = symbol "." <?> "dot"

keywords :: Set Text
keywords =
  S.fromList
    [ "λ",
      "∀",
      "let",
      "S",
      "natcase",
      "in",
      "if",
      "then",
      "else",
      "fix"
    ]

checkVar :: Text -> Parser Text
checkVar x =
  if x `S.member` keywords
    then fail $ "keyword " ++ show x ++ " cannot be an variable"
    else return x

varP :: Parser Text
varP = lexeme ((p >>= checkVar) <?> "variable")
  where
    p = fmap T.pack $ (:) <$> letterChar <*> many alphaNumChar

evarP :: Parser EVar
evarP = MkEVar <$> varP

tvarP :: Parser TVar
tvarP = MkTVar <$> varP

typeP :: Parser Type
typeP =
  paren typeP
    <|> tUnitP
    <|> tBoolP
    <|> tNatP
    <|> tAllP
    <|> try tArrP
    <|> TVar
    <$> tvarP
  where
    tUnitP = symbol "Unit" $> TUnit
    tBoolP = symbol "Bool" $> TBool
    tNatP = symbol "Nat" $> TNat
    tAllP = do
      symbol "∀"
      tyVar <- tvarP
      dotP
      TAll tyVar <$> typeP
    tArrP = do
      tyA <- typeP
      symbol "→"
      TArr tyA <$> typeP
