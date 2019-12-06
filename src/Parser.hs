{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Syntax.Expr
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

evarP :: Parser EVar
evarP = (lexeme . try) (p >>= check) <?> "Expression Variable"
  where
    p =
      fmap T.pack $
        (\us x xs -> us ++ x : xs)
          <$> many (char '_')
          <*> letterChar
          <*> many (char '_' <|> alphaNumChar)
    check x =
      if x `S.member` keywords
        then fail $ "keyword " ++ show x ++ " cannot be an expression variable"
        else return . MkEVar $ x
