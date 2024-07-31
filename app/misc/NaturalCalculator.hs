-- A calculator for natural expressions
module NaturalCalculator where

-- lexer, convert expression into tokens
data Op = 
    Plus 
    | Multiply 
    deriving (Show, Eq)

precedence :: Op -> Int
precedence Plus = 0
precedence Multiply = 1

data Token a = 
    Digit a
    | Operator Op
    | OpenParen
    | CloseParen
    deriving (Show, Eq)

readDigit :: Num a => Char -> Maybe a
readDigit '0' = Just 0
readDigit '1' = Just 1
readDigit '2' = Just 2
readDigit '3' = Just 3
readDigit '4' = Just 4
readDigit '5' = Just 5
readDigit '6' = Just 6
readDigit '7' = Just 7
readDigit '8' = Just 8
readDigit '9' = Just 9
readDigit _ = Nothing

lexChar :: Num a => Char -> Maybe (Token a)
lexChar '(' = Just OpenParen
lexChar ')' = Just CloseParen
lexChar '+' = Just (Operator Plus)
lexChar '*' = Just (Operator Multiply)
lexChar other = fmap Digit (readDigit other)

lexExpression :: Num a => String -> Maybe [Token a]
lexExpression = mapM lexChar

-- parse into abstract syntax tree
data Expression a = 
    Value a 
    | Expression (Expression a) Op (Expression a)
    deriving Show

-- combine all the starting tokens that are digits into a number, also returning remaining tokens
parseDigits :: Num a => a -> [Token a] -> (a, [Token a])
parseDigits prev ((Digit value) : remaining) = parseDigits (10 * prev + value) remaining
parseDigits prev remaining = (prev, remaining)

-- parse an expression, given the expression up until this point
-- returns the remaining tokens after parsing the expression
parseExpression' :: (Eq a, Num a) => Maybe (Expression a) -> Bool -> Bool -> [Token a] -> (Expression a, [Token a])
parseExpression' (Just expr) _ _ [] = (expr, [])
parseExpression' Nothing highPrecedence isFirstArg (Digit value : remaining) =
    let (number, remainingTokens) = parseDigits value remaining in
        if highPrecedence && not isFirstArg -- if highPrecedence then return immediately and let the expression with higher precedence evaluate the remaining tokens
            then (Value number, remainingTokens)
            else parseExpression' (Just $ Value number) highPrecedence False remainingTokens
-- todo: handle operator precedence, if there was an operator with higher precedence previously then we should end the expression here and return to it
-- need to handle cases like 1 + 3 * 2 along with 3 * 2 + 1
parseExpression' (Just leftExpr) highPrecedence _ (Operator op : remaining) = 
    let (rightExpr, remainingTokens) = parseExpression' Nothing (highPrecedence || op == Multiply) False remaining 
        operatorExpr = Expression leftExpr op rightExpr in
        if highPrecedence -- if highPrecedence then return immediately and let the expression with higher precedence evaluate the remaining tokens
            then (operatorExpr, remainingTokens)
            else parseExpression' (Just operatorExpr) False False remainingTokens
parseExpression' Nothing highPrecedence _ (OpenParen : remaining) = parseExpression' Nothing highPrecedence True remaining
parseExpression' (Just expr) _ _ (CloseParen : remaining) = (expr, remaining)
-- report incorrectly formed expressions
parseExpression' Nothing _ _ (Operator _ : _) = error "no previous expression for the operator"
parseExpression' (Just _) _ _ (Digit _ : _) = error "digit shouldn't have a previous expression"
parseExpression' (Just _) _ _ (OpenParen : _) = error "open parenthesis shouldn't have a previous expression"
parseExpression' Nothing _ _ (CloseParen : _) = error "close parenthesis is not matched with an open parenthesis"
parseExpression' Nothing _ _ [] = error "empty expression"
-- only operator needs the previous expression

parseExpression :: (Eq a, Num a) => [Token a] -> Expression a
parseExpression = fst . parseExpression' Nothing False True

-- evaluate an operation on its arguments
evalOp :: Num a => Op -> a -> a -> a
evalOp Plus = (+)
evalOp Multiply = (*)

-- evaluate an expression
evalExpression :: Num a => Expression a -> a
evalExpression (Value result) = result
evalExpression (Expression left op right) = evalOp op (evalExpression left) (evalExpression right)

-- parse and evaluate a string expression
eval :: (Eq a, Num a) => String -> Maybe a
eval = fmap (evalExpression . parseExpression) . lexExpression

testCases :: [(String, Int)]
testCases = [("1+3*2", 7),
    ("3*2+1", 7),
    ("1+3*2+1",8),
    ("11+3*(2+1)+2",22)]

testPasses :: (String, Int) -> Bool
testPasses (exprStr, result) = eval exprStr == Just result

calculatorMain :: IO ()
calculatorMain = do
    print $ "Tests: " ++ (if all testPasses testCases then "passed" else "failed")
    print $ "Expression: " ++ expression
    print lexed
    print (fmap parseExpression lexed)
    print (eval expression :: Maybe Int)
    where
        expression = "(11+3*(2+1+1)+2)+100" -- ("(11+3*(2+1+1)+2)+100",125) currently doesn't work
        lexed = lexExpression expression :: Maybe [Token Int]
