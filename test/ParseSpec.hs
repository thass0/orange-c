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
    it "incomplete" $ do
      runTestParseMaybe pExpr "-!!~!-" `shouldBe` Nothing
      runTestParseMaybe pExpr "1 + " `shouldBe` Nothing
      runTestParseMaybe pExpr "(* 5) + 1" `shouldBe` Nothing
      runTestParseMaybe pExpr "(1 - (71 + -)) * 4" `shouldBe` Nothing
      runTestParseMaybe pExpr "5 * ~!~ + ~~52" `shouldBe` Nothing
    it "in parentheses" $ do
      runTestParse pExpr "(523)" `shouldBe` Constant 523
      runTestParse pExpr "((((42))))" `shouldBe` Constant 42
      runTestParse pExpr "(43) + ((78))" `shouldBe`
        Add (Constant 43) (Constant 78)
      runTestParseMaybe pExpr "(((7))" `shouldBe` Nothing
      runTestParseMaybe (pExpr *> eof) "7 (- 2)" `shouldBe` Nothing
    it "simple binary" $ do
      runTestParse pExpr "1 + 2" `shouldBe` Add (Constant 1) (Constant 2)
      runTestParse pExpr "1 - 2" `shouldBe` Sub (Constant 1) (Constant 2)
      runTestParse pExpr "1 * 2" `shouldBe` Mul (Constant 1) (Constant 2)
      runTestParse pExpr "1 / 2" `shouldBe` Div (Constant 1) (Constant 2)
    it "chained binary" $ do
      runTestParse pExpr "1 + 2 + 3" `shouldBe`
        Add (Add (Constant 1) (Constant 2)) (Constant 3)
      runTestParse pExpr "1 - 2 - 3" `shouldBe`
        Sub (Sub (Constant 1) (Constant 2)) (Constant 3)
      runTestParse pExpr "1 * 2 * 3" `shouldBe`
        Mul (Mul (Constant 1) (Constant 2)) (Constant 3)
      runTestParse pExpr "1 / 2 / 3" `shouldBe`
        Div (Div (Constant 1) (Constant 2)) (Constant 3)
    it "mixed binary" $ do
      runTestParse pExpr "1 + 2 * 91" `shouldBe`
        Add (Constant 1) (Mul (Constant 2) (Constant 91))
      runTestParse pExpr "1 / 3 * 6 - 2" `shouldBe`
        Sub (Mul (Div (Constant 1) (Constant 3)) (Constant 6)) (Constant 2)
    it "parenthesized binary" $ do
      runTestParse pExpr "3 * (1 + 4) - 9" `shouldBe`
        Sub (Mul (Constant 3) (Add (Constant 1) (Constant 4))) (Constant 9)
      runTestParse pExpr "(1 - (71 + 10)) * 4" `shouldBe`
        Mul (Sub (Constant 1) (Add (Constant 71) (Constant 10))) (Constant 4)
    it "mixed expressions" $ do
      runTestParse pExpr "(-3 + !0) / ~~4" `shouldBe`
        Div (Add (Negate (Constant 3)) (LogicNot (Constant 0)))
        (BitNot (BitNot (Constant 4)))
      runTestParse pExpr "5 * ~!~1 + !!52" `shouldBe`
        Add (Mul (Constant 5) (BitNot (LogicNot (BitNot (Constant 1)))))
        (LogicNot (LogicNot (Constant 52)))
      runTestParse pExpr "9 - -1" `shouldBe`
        Sub (Constant 9) (Negate (Constant 1))


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
