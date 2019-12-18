{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.Combinators.Expr
  ( Operator (..),
    makeExprParser,
  )
import Data.Either.Extra (mapLeft)
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Syntax.Cons
import Syntax.Decl
import Syntax.Expr
import Syntax.Program (Program (..))
import Syntax.Type
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

programP :: Parser Program
programP = sc >> (Program <$> many (try declP) <*> exprP <* eof)

runProgramP :: String -> Text -> Either (ParseErrorBundle Text Void) Program
runProgramP path prog = mapLeft id $ runParser programP path prog

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
      "data",
      "S",
      "case",
      "natcase",
      "sumcase",
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

varP :: Parser Char -> Parser Text
varP start = lexeme (p >>= checkVar)
  where
    p = fmap T.pack $ (:) <$> start <*> many alphaNumChar

smallVarP :: Parser Text
smallVarP = varP lowerChar

bigVarP :: Parser Text
bigVarP = varP upperChar

evarP :: Parser EVar
evarP = (MkEVar <$> smallVarP) <?> "Expression variable"

tvarP :: Parser TVar
tvarP = (MkTVar <$> smallVarP) <?> "Type variable"

consVarP :: Parser ConsVar
consVarP = (MkConsVar <$> bigVarP) <?> "Constructor"

dataTypeP :: Parser Text
dataTypeP = bigVarP <?> "Data type"

typeTermP :: Parser Type
typeTermP =
  try (paren typeP)
    <|> try tProdP
    <|> tUnitP
    <|> tBoolP
    <|> tNatP
    <|> tAllP
    <|> tDataP
    <|> TVar
    <$> tvarP
  where
    tDataP = do
      tyName <- dataTypeP
      tyArgs <- many typeP
      return $ TData tyName tyArgs
    tUnitP = symbol "Unit" $> TUnit
    tBoolP = symbol "Bool" $> TBool
    tNatP = symbol "Nat" $> TNat
    tAllP = do
      symbol "∀"
      tyVar <- tvarP
      dotP
      TAll tyVar <$> typeP
    tProdP = do
      ty1 <- symbol "(" >> typeP
      ty2 <- symbol "," >> typeP
      symbol ")" $> TProd ty1 ty2

typeP :: Parser Type
typeP = makeExprParser typeTermP [[infixR "+" TSum], [infixR "→" TArr]]
  where
    infixR :: Text -> (Type -> Type -> Type) -> Operator Parser Type
    infixR name f = InfixR (f <$ symbol name)

exprTermP :: Parser Expr
exprTermP =
  try (paren exprP)
    <|> try eProdP
    <|> eUnitP
    <|> eTrueP
    <|> eFalseP
    <|> eZeroP
    <|> eSuccP
    <|> eNatCaseP
    <|> eInj1P
    <|> eInj2P
    <|> eSumCaseP
    <|> eLamP
    <|> eLetP
    <|> eIfP
    <|> eFixP
    <|> eConsP
    <|> try (EVar <$> evarP)
  where
    eProdP = do
      e1 <- symbol "(" >> exprP
      e2 <- symbol "," >> exprP
      symbol ")" $> EProd e1 e2
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
    eInj1P = EInj1 <$> (symbol "Inj1" >> exprP)
    eInj2P = EInj2 <$> (symbol "Inj2" >> exprP)
    eSumCaseP = do
      symbol "sumcase"
      e <- exprP
      (x, e1, y, e2) <- sumCaseBranchP
      return $ ESumCase e x e1 y e2
      where
        sumCaseBranchP = do
          x <- symbol "{" >> symbol "Inj1" >> evarP
          e1 <- symbol "→" >> exprP
          y <- symbol "," >> symbol "Inj2" >> evarP
          e2 <- symbol "→" >> exprP
          symbol "}"
          return (x, e1, y, e2)
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
    eConsP = do
      cons <- consVarP
      return $ ECons cons mempty

annotationP = symbol ":" >> typeP

exprP :: Parser Expr
exprP =
  makeExprParser
    exprTermP
    [[Postfix eProjP], [Postfix eTAppP], [InfixL (EApp <$ space)], [Postfix eAnnoP]]
  where
    eAnnoP = do
      ty <- annotationP
      return $ \e -> EAnno e ty
    eProjP = (symbol ".1" $> EProj1) <|> (symbol ".2" $> EProj2)
    eTAppP = do
      tyArg <- symbol "@" >> typeP
      return $ \e -> ETApp e tyArg

declP :: Parser Declaration
declP = do
  typeName <- symbol "data" >> dataTypeP
  tyVars <- many tvarP
  symbol "="
  conss <- sepBy1 constructorP (symbol "|")
  dotP
  return $ Declaration typeName tyVars conss

constructorP :: Parser Constructor
constructorP = do
  consVar <- consVarP
  tyArgs <- many typeP
  return $ Constructor consVar tyArgs
