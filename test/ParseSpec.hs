module ParseSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Data.Text

import Parse
import Ast

spec :: Spec
spec = do
  describe "Parsing" $ do
    expressions

expressions :: Spec
expressions = do
  describe "expressions" $ do
    it "simple unary" $ do
      runTestParse pExpr "-1" `shouldBe` Negate (Constant 1)
      runTestParse pExpr "!2" `shouldBe` LogicNot (Constant 2)
      runTestParse pExpr "~3" `shouldBe` BitNot (Constant 3)
    it "chained unary" $ do
      runTestParse pExpr "---1" `shouldBe` Negate (Negate (Negate (Constant 1)))
      runTestParse pExpr "!!2" `shouldBe` LogicNot (LogicNot (Constant 2))
      runTestParse pExpr "~~~~~3" `shouldBe`
        BitNot (BitNot (BitNot (BitNot (BitNot (Constant 3)))))
    it "mixed chained unary" $ do
      runTestParse pExpr "-!~1" `shouldBe`
        Negate (LogicNot (BitNot (Constant 1)))
      runTestParse pExpr "~~!-!-2" `shouldBe`
        BitNot (BitNot (LogicNot (Negate (LogicNot (Negate (Constant 2))))))
      runTestParse pExpr "~~~!~3" `shouldBe`
        BitNot (BitNot (BitNot (LogicNot (BitNot (Constant 3)))))
    it "missing constant" $
      runTestParseMaybe pExpr "--!!~!-" `shouldBe` Nothing

-- * Helpers

runTestParse :: Parser a -> Text -> a
runTestParse parser input =
  case runTestParse' parser input of
    Right x -> x
    Left err -> error err

runTestParseMaybe :: Parser a -> Text -> Maybe a
runTestParseMaybe parser input =
  case runTestParse' parser input of
    Right x -> Just x
    Left _ -> Nothing

runTestParse' :: Parser a -> Text -> Either String a
runTestParse' parser input =
  case runParser parser "<file>" input of
    Right x -> Right x
    Left err -> Left (errorBundlePretty err)
