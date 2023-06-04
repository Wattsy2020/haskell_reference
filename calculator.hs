-- Implement a reverse polish notation calculator for simple math operations
data Op = Plus | Minus | Multiply deriving (Show)

data Token a = Number a | Operator Op deriving (Show)

parseToken :: Read a => String -> Token a
parseToken "+" = Operator Plus
parseToken "-" = Operator Minus
parseToken "*" = Operator Multiply
parseToken number = Number (read number)

parseExpression :: Read a => String -> [Token a]
parseExpression = map parseToken . words

opToFunc :: Num a => Op -> (a -> a -> a)
opToFunc Plus = (+)
opToFunc Minus = (-)
opToFunc Multiply = (*)

evalToken :: Num a => [a] -> Token a -> [a]
evalToken stack (Number num) = num : stack
evalToken (first : second : stack) (Operator op) = opToFunc op second first : stack

evalExpression :: Num a => [Token a] -> a
evalExpression (Number num : tokens) = head $ foldl evalToken [num] tokens

evalExpr :: (Num a, Read a) => String -> a
evalExpr = evalExpression . parseExpression

main = do
  print $ evalExpr "10 4 3 + 2 * -"
  print $ evalExpr "90 34 12 33 55 66 + * - + -"