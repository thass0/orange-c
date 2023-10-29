module Parse
  ( Parse.parse
  , Parser
  , pIdent
  , pInteger
  , pUnary
  , pFactor
  , pTerm
  , pExpr
  , pStatement
  , pStatements
  , pProgram
  )
where

import Data.List.NonEmpty
import Control.Monad
import Control.Applicative hiding (many, some)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text hiding (singleton, foldr1, foldl)
import Data.Functor (($>))

import Ast

parse :: FilePath -> Text -> Either String Ast
parse file src = case runParser pProgram file src of
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

-- | Consume the given token of text and the required trailing whitespace.
symbol' ::  Text -> Parser Text
symbol' s= do
  void (string s)
  -- Mandates whitespace but removes comments, too.
  Parse.lexeme space1
  pure s

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
            <?> "identifier"

-- | Parse an integer constant.
pInteger :: Parser Expression
pInteger = Constant <$> Parse.lexeme L.decimal

-- | Parse a unary operator that expects to receive another expression.
pUnary :: Parser (Expression -> Expression)
pUnary = choice
  [ Parse.symbol "-" $> Negate
  , Parse.symbol "!" $> LogicNot
  , Parse.symbol "~" $> BitNot
  ]

-- | Parse a factor in a term.
--   <factor> -> "(" <expr> ")" | <unary> <factor> | <constant>
pFactor :: Parser Expression
pFactor = choice
  [ inParens pExpr
  , pUnary <*> pFactor
  , pInteger
  ]

-- | Parse a term in an expression.
--   <term> -> <factor { ("*" | "/") <factor }
pTerm :: Parser Expression
pTerm = do
  lhs <- pFactor
  rhs <- many $ (pMul <|> pDiv) <*> pFactor
  pure $ foldl (\expr op -> op expr) lhs rhs
  where
    pMul = Parse.symbol "*" $> flip Mul
    pDiv = Parse.symbol "/" $> flip Div

-- | Parse an expression.
--   <expr> -> <term> { ("+" | "-") <term> }
pExpr :: Parser Expression
pExpr = do
  lhs <- pTerm
  rhs <- many $ (pAdd <|> pSub) <*> pTerm
  pure $ foldl (\expr op -> op expr) lhs rhs
  where
    -- `flip` is needed to order the operands correctly,
    -- since the first term that's applied to `pAdd` or
    -- `pSub` is the right hand side of the operation.
    pAdd = Parse.symbol "+" $> flip Add
    pSub = Parse.symbol "-" $> flip Sub

-- | Parse a single statement.
pStatement :: Parser Statement
pStatement = do
  void $ Parse.symbol' "return"
  expr <- pExpr
  void $ Parse.symbol ";"
  pure $ Return expr

-- | Parse one or more statements.
pStatements :: Parser (NonEmpty Statement)
pStatements = fromList <$> some pStatement

-- | Parse the main function.
pProgram :: Parser Ast
pProgram = do
  void Text.Megaparsec.Char.space
  void $ Parse.symbol' "int"
  ident <- pIdent
  void $ inParens (optional (Parse.symbol "void"))
  statements <- inBraces pStatements
  pure $ Program (singleton (Function ident statements))
