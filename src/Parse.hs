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

-- | Parse a left-associative binary expression.
binExprL
  -- | A side of the equation.
  :: Parser Expression
  -- | The operator. Returns a function that given both sides of
  --   the expression will return the final one.
  -> Parser (Expression -> Expression -> Expression)
  -> Parser Expression
binExprL pSide operator = do
  lhs <- pSide
  -- The first side that will be applied to the function inside
  -- the `operator` parser is the right hand side, but because
  -- the first parameter of the constructor should be the left
  -- hand side the parameters must be flipped.
  rhs <- many $ flip <$> operator <*> pSide
  pure $ foldl (\expr op -> op expr) lhs rhs
  

-- | Parse an identifier.
pIdent :: Parser Text
pIdent = Parse.lexeme $ fmap pack ident
  where
    ident = (:)
            <$> (letterChar <|> char '_')
            <*> many (alphaNumChar <|> char '_')
            <?> "identifier"

-- | Parse an integer constant.
pInteger :: Parser Expression
pInteger = Constant <$> Parse.lexeme L.decimal

-- | <unary> -> ("-" | "!" | "~") <factor>
pUnary :: Parser Expression
pUnary = (pNegate <|> pLogicNot <|> pBitNot) <*> pFactor
  where
    pNegate = Parse.symbol "-" $> Negate
    pLogicNot = Parse.symbol "!" $> LogicNot
    pBitNot = Parse.symbol "~" $> BitNot

-- | <factor> -> "(" <expr> ")" | <unary> | <constant>
pFactor :: Parser Expression
pFactor = inParens pExpr <|> pUnary <|> pInteger

-- | <term> -> <factor> { ("*" | "/") <factor> }
pTerm :: Parser Expression
pTerm = binExprL pFactor (pMul <|> pDiv <|> pMod)
  where
    pMul = Parse.symbol "*" $> Mul
    pDiv = Parse.symbol "/" $> Div
    pMod = Parse.symbol "%" $> Mod

-- | <arith-expr> -> <term> { ("+" | "-") <term> }
pArithExpr :: Parser Expression
pArithExpr = binExprL pTerm (pAdd <|> pSub)
  where
    pAdd = Parse.symbol "+" $> Add
    pSub = Parse.symbol "-" $> Sub

-- | <shift-expr> -> <arith-expr> { ("<<" | ">>") <arith-expr> }
pShiftExpr :: Parser Expression
pShiftExpr = binExprL pArithExpr (pLeftShift <|> pRightShift)
  where
    pLeftShift = Parse.symbol "<<" $> LeftShift
    pRightShift = Parse.symbol ">>" $> RightShift

-- | <rel-cmp-expr> ->
--   <shift-expr> { ("<=" | "<" | ">=" | ">") <shift-expr> }
pRelCmpExpr :: Parser Expression
pRelCmpExpr = binExprL pShiftExpr
  -- Note the ordering here. If `pG` (e.g.) came before
  -- `pGe`, it would parse the operator `<=` as only a
  -- `<` leaving behind the `=`.
  (pLeq <|> pL <|> pGeq <|> pG)
  where
    pLeq = Parse.symbol "<=" $> RelLeq
    pL = Parse.symbol "<" $> RelL
    pGeq = try (Parse.symbol' ">=") $> RelGeq
    pG = Parse.symbol ">" $> RelG

-- | <rel-eq-expr> -> <rel-cmp-expr> { ("==" | "!=") <rel-cmp-expr> }
pRelEqExpr :: Parser Expression
pRelEqExpr = binExprL pRelCmpExpr (pEq <|> pNeq)
  where
    pEq = Parse.symbol "==" $> RelEq
    pNeq = Parse.symbol "!=" $> RelNeq

-- | <bit-and-expr> -> <rel-eq-expr> { "&" <rel-eq-expr> }
pBitAndExpr :: Parser Expression
pBitAndExpr = binExprL pRelEqExpr (pBitAnd $> BitAnd)
  where
    -- `&` is a higher-precedence prefix of `&&`.
    pBitAnd = try (pBitAnd' <* notFollowedBy pBitAnd')
    pBitAnd' = Parse.symbol "&"

-- | <bit-xor-exp> -> <bit-and-expr> { "^" <bit-and-expr> }
pBitXorExpr :: Parser Expression
pBitXorExpr = binExprL pBitAndExpr (Parse.symbol "^" $> BitXor)

-- | <bit-or-expr> -> <bit-xor-expr> { "|" <bit-xor-expr> }
pBitOrExpr :: Parser Expression
pBitOrExpr = binExprL pBitXorExpr (pBitOr $> BitOr)
  where
    -- `|` is a higher-precedence prefix of `||`.
    pBitOr = try (pBitOr' <* notFollowedBy pBitOr')
    pBitOr' = Parse.symbol "|"

-- | <logic-and-expr> -> <bit-or-expr> { "&&" <bit-or-expr> }
pLogicAndExpr :: Parser Expression
pLogicAndExpr = binExprL pBitOrExpr (Parse.symbol "&&" $> LogicAnd)

-- | <logic-or-expr> -> <logic-and-expr> { "||" <logic-and-expr> }
pLogicOrExpr :: Parser Expression
pLogicOrExpr = binExprL pLogicAndExpr (Parse.symbol "||" $> LogicOr)

-- | <expr> -> <logic-or-expr>
pExpr :: Parser Expression
pExpr = pLogicOrExpr

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
