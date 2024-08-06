-- solves https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/description/
module MaximumParenthesesDepth where

-- given a new character, return the affect on the parent depth
updateDepth :: Char -> Int
updateDepth '(' = 1
updateDepth ')' = -1
updateDepth _ = 0

-- find the maximum depth of parenthesis
maxDepth :: String -> Int
maxDepth = maximum . scanl (+) 0 . map updateDepth
