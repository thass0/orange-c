module Parse
  ( Ast
  , Program (..)
  , Function (..)
  , Ident
  , Statement (..)
  , Expression (..)
  , Parse.parse
  ) where

import Data.List.NonEmpty
import Control.Monad
import Control.Applicative hiding (many, some)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text hiding (singleton)

type Ast = Program

data Program = Program (NonEmpty Function)
  deriving (Show, Eq)
-- There is no distinction here between a function
-- declaration and its definition. This distinction
-- purely made while parsing and is not reflected in
-- the AST.
data Function = Function Ident (NonEmpty Statement)
  deriving (Show, Eq)

type Ident = Text

data Statement = Return Expression
  deriving (Show, Eq)

data Expression = Constant Int
  deriving (Show, Eq)

parse :: FilePath -> Text -> Either String Ast
parse file src = case runParser pMain file src of
  Left err -> Left $ errorBundlePretty err
  Right ast -> Right ast

type Parser = Parsec Void Text

-- | Consume as many whitespace characters and comments as possible.
spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")

-- | Consume the given token of text and trailing whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- | Run the given parser and consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Run the given parser enclosed in parentheses.
inParens :: Parser a -> Parser a
inParens = between (Parse.symbol "(") (Parse.symbol ")")

-- | Run the given parser enclosed in braces.
inBraces :: Parser a -> Parser a
inBraces = between (Parse.symbol "{") (Parse.symbol "}")

-- | Parse an identifier.
pIdent :: Parser Text
pIdent =
  Parse.lexeme $ fmap pack ident
  where
    ident = (:) <$> (letterChar <|> char '_')
            <*> many (alphaNumChar <|> char '_')

-- | Parse an integer constant.
pInteger :: Parser Int
pInteger = Parse.lexeme L.decimal

-- | Parse a single statement.
pStatement :: Parser Statement
pStatement = do
  void $ Parse.symbol "return"
  i <- pInteger
  void $ Parse.symbol ";"
  pure $ Return (Constant i)

-- | Parse one or more statements.
pStatements :: Parser (NonEmpty Statement)
pStatements = fromList <$> some pStatement

-- | Parse the main function.
pMain :: Parser Ast
pMain = do
  void Text.Megaparsec.Char.space
  void $ Parse.symbol "int"
  ident <- pIdent
  void $ inParens (Parse.symbol "void")
  statements <- inBraces pStatements
  pure $ Program (singleton (Function ident statements))
