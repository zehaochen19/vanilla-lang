{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Either.Extra (mapLeft)
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

programP :: Parser Expr
programP = exprP <* eof

runProgramP :: String -> Text -> Either String Expr
runProgramP path prog = mapLeft show $ runParser programP path prog

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
      "fix",
      "Nat",
      "Bool",
      "Unit"
    ]

checkVar :: Text -> Parser Text
checkVar x =
  if x `S.member` keywords
    then fail $ "keyword " ++ T.unpack x ++ " cannot be an variable"
    else return x

varP :: Parser Text
varP = lexeme ((p >>= checkVar) <?> "variable")
  where
    p = fmap T.pack $ (:) <$> letterChar <*> many alphaNumChar

evarP :: Parser EVar
evarP = MkEVar <$> varP

tvarP :: Parser TVar
tvarP = MkTVar <$> varP

typeTermP :: Parser Type
typeTermP =
  try (paren typeP)
    <|> tUnitP
    <|> tBoolP
    <|> tNatP
    <|> tAllP
    <|> TVar <$> tvarP
  where
    tUnitP = symbol "Unit" $> TUnit
    tBoolP = symbol "Bool" $> TBool
    tNatP = symbol "Nat" $> TNat
    tAllP = do
      symbol "∀"
      tyVar <- tvarP
      dotP
      TAll tyVar <$> typeP

typeP :: Parser Type
typeP = makeExprParser typeTermP [[infixR "→" TArr]]
  where
    infixR :: Text -> (Type -> Type -> Type) -> Operator Parser Type
    infixR name f = InfixR (f <$ symbol name)

exprTermP :: Parser Expr
exprTermP =
  try (paren exprP)
    <|> eUnitP
    <|> eTrueP
    <|> eFalseP
    <|> eZeroP
    <|> eSuccP
    <|> eNatCaseP
    <|> eLamP
    <|> eLetP
    <|> eIfP
    <|> eFixP
    <|> try (EVar <$> evarP)
  where
    eUnitP = symbol "()" $> EUnit
    eTrueP = symbol "True" $> ETrue
    eFalseP = symbol "False" $> EFalse
    eZeroP = symbol "0" $> EZero
    eSuccP = ESucc <$> (symbol "S" *> exprP)
    eNatCaseP = do
      symbol "natcase"
      n <- exprP
      (e1, x, e2) <- natCaseBranchP
      return $ ENatCase n e1 x e2
      where
        natCaseBranchP = do
          e1 <- symbol "{" >> symbol "0" >> symbol "→" >> exprP
          x <- symbol "," >> symbol "S" >> evarP
          e2 <- symbol "→" >> exprP
          symbol "}"
          return (e1, x, e2)
    eLamP = do
      x <- symbol "λ" >> evarP
      annotation <- optional annotationP
      body <- dotP >> exprP
      return $ case annotation of
        Nothing -> ELam x body
        Just ty -> EALam x ty body
    eLetP = do
      x <- symbol "let" >> evarP
      anno <- optional annotationP
      e1 <- symbol "=" >> exprP
      e2 <- symbol "in" >> exprP
      return $ case anno of
        Nothing -> ELet x e1 e2
        Just ty -> EALet x ty e1 e2
    eIfP = do
      b <- symbol "if" >> exprP
      e1 <- symbol "then" >> exprP
      e2 <- symbol "else" >> exprP
      return $ EIf b e1 e2
    eFixP = symbol "fix" >> (EFix <$> exprP)

annotationP = symbol ":" >> typeP

exprP :: Parser Expr
exprP =
  makeExprParser
    exprTermP
    [ [InfixL (EApp <$ space)],
      [Postfix eAnnoP]
    ]
  where
    eAnnoP = do
      ty <- annotationP
      return $ \e -> EAnno e ty
