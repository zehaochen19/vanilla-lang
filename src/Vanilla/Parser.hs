{-# LANGUAGE OverloadedStrings #-}

module Vanilla.Parser where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Data.Either.Extra              ( mapLeft )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( isJust )
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Vanilla.Syntax.Cons
import           Vanilla.Syntax.Decl
import           Vanilla.Syntax.Expr
import           Vanilla.Syntax.Program         ( Program(..) )
import           Vanilla.Syntax.Type

type Parser = Parsec Void Text

programP :: Parser Program
programP = sc $> Program <*> many declP <*> exprP <* eof

runProgramP :: String -> Text -> Either (ParseErrorBundle Text Void) Program
runProgramP path prog = mapLeft id $ runParser programP path prog

sc :: Parser ()
sc = L.space space1 lineComment blockComment
 where
  lineComment  = L.skipLineComment "--"
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
keywords = S.fromList
  ["λ", "∀", "let", "forall", "data", "case", "rec", "of", "in", "fix"]

checkVar :: Text -> Parser Text
checkVar x = if x `S.member` keywords
  then fail $ "keyword " ++ T.unpack x ++ " cannot be an variable"
  else return x

varP :: Parser Char -> Parser Text
varP start = lexeme (p >>= checkVar)
  where p = fmap T.pack $ (:) <$> start <*> many alphaNumChar

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

lambdaP :: Parser ()
lambdaP = () <$ (symbol "λ" <|> symbol "\\")

forallP :: Parser ()
forallP = () <$ (symbol "∀" <|> symbol "forall")

arrowP :: Parser ()
arrowP = () <$ (symbol "→" <|> symbol "->")


typeTermP :: Parser Type
typeTermP = paren typeP <|> tAllP <|> tDataP <|> TVar <$> tvarP
 where
  tDataP = TData <$> dataTypeP <*> (Seq.fromList <$> many typeP)
  tAllP  = TAll <$> (forallP *> tvarP) <*> (dotP *> typeP)

typeP :: Parser Type
typeP = makeExprParser typeTermP [[infixR "→" TArr]]
 where
  infixR :: Text -> (Type -> Type -> Type) -> Operator Parser Type
  infixR name f = InfixR (f <$ symbol name)

exprTermP :: Parser Expr
exprTermP =
  paren exprP <|> eLamP <|> eLetP <|> eFixP <|> eConsP <|> eCaseP <|> try
    (EVar <$> evarP)
 where
  eLamP = do
    x          <- lambdaP >> evarP
    annotation <- optional annotationP
    body       <- dotP >> exprP
    return $ case annotation of
      Nothing -> ELam x body
      Just ty -> EALam x ty body
  eLetP = do
    symbol "let"
    rec' <- optional (symbol "rec")
    x    <- evarP
    anno <- optional annotationP
    e1   <- symbol "=" >> exprP
    e2   <- symbol "in" >> exprP
    return $ case anno of
      Nothing -> ELet x e1 e2
      Just ty -> (if isJust rec' then EALetRec else EALet) x ty e1 e2
  eFixP  = symbol "fix" *> (EFix <$> exprP)
  eConsP = ECons <$> consVarP <*> pure mempty
  eCaseP =
    ECase
      <$> (symbol "case" *> exprP)
      <*> (symbol "of" *> symbol "{" *> sepBy1 branchP (symbol ","))
      <*  symbol "}"

branchP :: Parser Branch
branchP = Branch <$> consVarP <*> many evarP <*> (arrowP *> exprP)

annotationP = symbol ":" >> typeP

exprP :: Parser Expr
exprP = makeExprParser
  exprTermP
  [[Postfix eTAppP], [InfixL (EApp <$ space)], [Postfix eAnnoP]]
 where
  eAnnoP = do
    ty <- annotationP
    return $ \e -> EAnno e ty
  eTAppP = do
    tyArg <- symbol "@" >> typeP
    return $ \e -> ETApp e tyArg

declP :: Parser Declaration
declP =
  Declaration
    <$> (symbol "data" *> dataTypeP)
    <*> many tvarP
    <*> (symbol "=" *> sepBy1 constructorP (symbol "|"))
    <*  dotP

constructorP :: Parser Constructor
constructorP = Constructor <$> consVarP <*> many typeP
