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
  let parseExpr = runTestParse pExpr
  describe "expressions" $ do
    it "simple unary" $ do
      parseExpr "-1" `shouldBe` Negate (Constant 1)
      parseExpr "!2" `shouldBe` LogicNot (Constant 2)
      parseExpr "~3" `shouldBe` BitNot (Constant 3)

    it "chained unary" $ do
      parseExpr "!!2" `shouldBe`
        LogicNot (LogicNot (Constant 2))
      parseExpr "~~~~~3" `shouldBe`
        BitNot (BitNot (BitNot (BitNot (BitNot (Constant 3)))))

    it "mixed chained unary" $ do
      parseExpr "-!~1" `shouldBe`
        Negate (LogicNot (BitNot (Constant 1)))
      parseExpr "~~!-!-2" `shouldBe`
        BitNot
        (BitNot (LogicNot (Negate (LogicNot (Negate (Constant 2))))))
      parseExpr "~~~!~3" `shouldBe`
        BitNot (BitNot (BitNot (LogicNot (BitNot (Constant 3)))))

    it "incomplete" $ do
      runTestParseMaybe pExpr "-!!~!-" `shouldBe` Nothing
      runTestParseMaybe pExpr "1 + " `shouldBe` Nothing
      runTestParseMaybe pExpr "(* 5) + 1" `shouldBe` Nothing
      runTestParseMaybe pExpr "(1 - (71 + -)) * 4" `shouldBe` Nothing
      runTestParseMaybe pExpr "5 * ~!~ + ~~52" `shouldBe` Nothing

    it "in parentheses" $ do
      parseExpr "(523)" `shouldBe` Constant 523
      parseExpr "((((42))))" `shouldBe` Constant 42
      parseExpr "(43) + ((78))" `shouldBe`
        Add (Constant 43) (Constant 78)
      runTestParseMaybe pExpr "(((7))" `shouldBe` Nothing
      runTestParseMaybe (pExpr *> eof) "7 (- 2)" `shouldBe` Nothing

    it "simple binary" $ do
      let pair x = x (Constant 1) (Constant 2)
      parseExpr "1 + 2"  `shouldBe` pair Add
      parseExpr "1 - 2"  `shouldBe` pair Sub
      parseExpr "1 * 2"  `shouldBe` pair Mul
      parseExpr "1 / 2"  `shouldBe` pair Div
      parseExpr "1 % 2"  `shouldBe` pair Mod
      parseExpr "1 << 2" `shouldBe` pair LeftShift
      parseExpr "1 >> 2" `shouldBe` pair RightShift
      parseExpr "1 && 2" `shouldBe` pair LogicAnd
      parseExpr "1 || 2" `shouldBe` pair LogicOr
      parseExpr "1 & 2"  `shouldBe` pair BitAnd
      parseExpr "1 | 2"  `shouldBe` pair BitOr
      parseExpr "1 ^ 2"  `shouldBe` pair BitXor
      parseExpr "1 == 2" `shouldBe` pair RelEq
      parseExpr "1 != 2" `shouldBe` pair RelNeq
      parseExpr "1 <= 2" `shouldBe` pair RelLeq
      parseExpr "1 < 2"  `shouldBe` pair RelL
      parseExpr "1 >= 2" `shouldBe` pair RelGeq
      parseExpr "1 > 2"  `shouldBe` pair RelG

    it "chained binary" $ do
      let chainL x = x (x (Constant 1) (Constant 2)) (Constant 3)
      parseExpr "1 + 2 + 3"   `shouldBe` chainL Add
      parseExpr "1 - 2 - 3"   `shouldBe` chainL Sub
      parseExpr "1 * 2 * 3"   `shouldBe` chainL Mul
      parseExpr "1 / 2 / 3"   `shouldBe` chainL Div
      parseExpr "1 % 2 % 3"   `shouldBe` chainL Mod
      parseExpr "1 << 2 << 3" `shouldBe` chainL LeftShift
      parseExpr "1 >> 2 >> 3" `shouldBe` chainL RightShift
      parseExpr "1 && 2 && 3" `shouldBe` chainL LogicAnd
      parseExpr "1 || 2 || 3" `shouldBe` chainL LogicOr
      parseExpr "1 & 2 & 3"   `shouldBe` chainL BitAnd
      parseExpr "1 | 2 | 3"   `shouldBe` chainL BitOr
      parseExpr "1 ^ 2 ^ 3"   `shouldBe` chainL BitXor
      parseExpr "1 == 2 == 3" `shouldBe` chainL RelEq
      parseExpr "1 != 2 != 3" `shouldBe` chainL RelNeq
      parseExpr "1 <= 2 <= 3" `shouldBe` chainL RelLeq
      parseExpr "1 < 2 < 3"   `shouldBe` chainL RelL
      parseExpr "1 >= 2 >= 3" `shouldBe` chainL RelGeq
      parseExpr "1 > 2 > 3"   `shouldBe` chainL RelG

    it "mixed binary" $ do
      parseExpr "1 + 2 * 91" `shouldBe`
        Add (Constant 1) (Mul (Constant 2) (Constant 91))
      parseExpr "1 / 3 * 6 - 2" `shouldBe`
        Sub
        (Mul
         (Div (Constant 1) (Constant 3))
         (Constant 6))
        (Constant 2)
      parseExpr "1 << 4 + 2 & 9 == 1" `shouldBe`
        BitAnd
        (LeftShift (Constant 1) (Add (Constant 4) (Constant 2)))
        (RelEq (Constant 9) (Constant 1))
      parseExpr "92 ^ 9 != 515 >> 19 || 78 % 6467" `shouldBe`
        LogicOr
        (BitXor
         (Constant 92)
         (RelNeq
          (Constant 9)
          (RightShift (Constant 515) (Constant 19))))
        (Mod (Constant 78) (Constant 6467))
      parseExpr "1 && 6 >= 962 == 0" `shouldBe`
        LogicAnd
        (Constant 1)
        (RelEq
         (RelGeq (Constant 6) (Constant 962))
         (Constant 0))

    it "parenthesized binary" $ do
      parseExpr "3 * (1 + 4) - 9" `shouldBe`
        Sub
        (Mul (Constant 3) (Add (Constant 1) (Constant 4)))
        (Constant 9)
      parseExpr "(1 - (71 + 10)) * 4" `shouldBe`
        Mul
        (Sub (Constant 1) (Add (Constant 71) (Constant 10)))
        (Constant 4)
      parseExpr "1 + 2 * 4 || (8/4) || 7 && 0" `shouldBe`
        LogicOr
        (LogicOr
          (Add
           (Constant 1)
           (Mul (Constant 2) (Constant 4)))
          (Div (Constant 8) (Constant 4)))
        (LogicAnd (Constant 7) (Constant 0))

    it "mixed expressions" $ do
      parseExpr "(-3 + !0) / ~~4" `shouldBe`
        Div
        (Add (Negate (Constant 3)) (LogicNot (Constant 0)))
        (BitNot (BitNot (Constant 4)))
      parseExpr "5 * ~!~1 + !!52" `shouldBe`
        Add
        (Mul (Constant 5) (BitNot (LogicNot (BitNot (Constant 1)))))
        (LogicNot (LogicNot (Constant 52)))
      parseExpr "9 - -1" `shouldBe`
        Sub (Constant 9) (Negate (Constant 1))
      parseExpr "~1 + 2 > 5 * 4 || (8/-4) || 7 && !0" `shouldBe`
        LogicOr
        (LogicOr
          (RelG
            (Add (BitNot (Constant 1)) (Constant 2))
            (Mul (Constant 5) (Constant 4)))
          (Div (Constant 8) (Negate (Constant 4))))
        (LogicAnd (Constant 7) (LogicNot (Constant 0)))
      parseExpr "(92 ^ 9) != 515 >> ~19 || 78 % -467" `shouldBe`
        LogicOr
        (RelNeq
         (BitXor (Constant 92) (Constant 9))
         (RightShift (Constant 515) (BitNot (Constant 19))))
        (Mod (Constant 78) (Negate (Constant 467)))
    it "mixed expressions without whitespace" $ do
      parseExpr "(-3+!0)/~~4" `shouldBe`
        Div
        (Add (Negate (Constant 3)) (LogicNot (Constant 0)))
        (BitNot (BitNot (Constant 4)))
      parseExpr "51&5*~!~1+!!52" `shouldBe`
        BitAnd
        (Constant 51)
        (Add
         (Mul (Constant 5) (BitNot (LogicNot (BitNot (Constant 1)))))
         (LogicNot (LogicNot (Constant 52))))
      parseExpr "9--1" `shouldBe`
        Sub (Constant 9) (Negate (Constant 1))
      parseExpr "~1+2>5*4||(8/-4)||7&&!0" `shouldBe`
        LogicOr
        (LogicOr
          (RelG
            (Add (BitNot (Constant 1)) (Constant 2))
            (Mul (Constant 5) (Constant 4)))
          (Div (Constant 8) (Negate (Constant 4))))
        (LogicAnd (Constant 7) (LogicNot (Constant 0)))
      parseExpr "(92^9)!=515>>~19||78%-467|8" `shouldBe`
        LogicOr
        (RelNeq
         (BitXor (Constant 92) (Constant 9))
         (RightShift (Constant 515) (BitNot (Constant 19))))
        (BitOr
         (Mod (Constant 78) (Negate (Constant 467)))
         (Constant 8))

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
