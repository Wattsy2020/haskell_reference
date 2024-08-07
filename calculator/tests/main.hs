{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.Maybe (mapMaybe)
import NaturalCalculator
import Test.Hspec
import Test.QuickCheck
import Data.Function (on)

testCases :: [(String, Int)]
testCases =
  [ ("1+3*2", 7),
    ("3*2+1", 7),
    ("1+3*2+1", 8),
    ("11+3*(2+1)+2", 22),
    ("1+3*(1+3*2)", 22),
    ("1+3*(1+3*2)+1", 23),
    ("1+3*(1+3*2+1)+1", 26),
    ("0+(11+3*(2+1+1)+2)+100", 125),
    ("(1)+1", 2),
    ("(11+3*(2+1-1)-2)+100", 115),
    ("-4*2", -8)
  ]

testPasses :: (String, Int) -> Bool
testPasses (exprStr, result) = eval exprStr == Right result

getFailedTest :: (String, Int) -> Maybe (String, Int)
getFailedTest (exprStr, result)
  | testPasses (exprStr, result) = Nothing
  | otherwise = Just (show $ fmap parseExpression (lexExpression exprStr :: Either ParseError [Token Int]), result)

instance Arbitrary Op where
  arbitrary :: Gen Op
  arbitrary = elements [ Plus, Multiply, Minus]

instance (Arbitrary a) => Arbitrary (Expression a) where
  arbitrary :: Gen (Expression a)
  arbitrary = frequency [
    (1, Value <$> (arbitrary :: Gen a)),
    (1, Expression <$> (arbitrary :: Gen (Expression a)) <*> (arbitrary :: Gen Op) <*> (arbitrary :: Gen (Expression a)))]

  shrink :: Expression a -> [Expression a]
  shrink (Value _) = []
  shrink (Expression leftExpr _ rightExpr) = [leftExpr, rightExpr]

prop_serializeroundtrip :: (Show a, Eq a, Num a) => Expression a -> Bool
prop_serializeroundtrip expr = case readExpression $ serializeExpression expr of
  Left _ -> False
  Right readExpr -> ((==) `on` evalExpression) expr readExpr

main :: IO ()
main = hspec $ do
  describe "Calculator" $ do
    it "Passes Manual Test cases" $ do
      mapMaybe getFailedTest testCases `shouldBe` []
    it "Passes Property Test cases" $ property (prop_serializeroundtrip :: Expression Int -> Bool)
