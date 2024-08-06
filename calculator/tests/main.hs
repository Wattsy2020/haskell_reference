module Main where

import Test.Hspec
import Data.Maybe (mapMaybe)
import NaturalCalculator

testCases :: [(String, Int)]
testCases = [("1+3*2", 7),
    ("3*2+1", 7),
    ("1+3*2+1",8),
    ("11+3*(2+1)+2",22),
    ("1+3*(1+3*2)",22),
    ("1+3*(1+3*2)+1",23),
    ("1+3*(1+3*2+1)+1",26),
    ("0+(11+3*(2+1+1)+2)+100",125),
    ("(1)+1", 2),
    ("(11+3*(2+1-1)-2)+100",115),
    ("-4*2", -8)]

testPasses :: (String, Int) -> Bool
testPasses (exprStr, result) = eval exprStr == Right result

getFailedTest :: (String, Int) -> Maybe (String, Int)
getFailedTest (exprStr, result)
    | testPasses (exprStr, result) = Nothing
    | otherwise = Just (show $ fmap parseExpression (lexExpression exprStr :: Either ParseError [Token Int]), result)

main :: IO ()
main = hspec $ do
  describe "Calculator" $ do
    it "Passes Manual Test cases" $ do
      mapMaybe getFailedTest testCases `shouldBe` []
