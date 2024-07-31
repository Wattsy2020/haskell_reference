module RichestCustomerWealth where

-- problem taken from https://leetcode.com/problems/richest-customer-wealth/description/
wealths :: [[Int]]
wealths = [[2,8,7],[7,1,3],[1,9,5]]

richestWealth :: [[Int]] -> Int
richestWealth = maximum . map sum

answer :: Int
answer = richestWealth wealths