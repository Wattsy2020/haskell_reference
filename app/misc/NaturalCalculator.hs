-- A calculator for natural expressions
module NaturalCalculator where

-- lexer, convert expression into tokens
data Op = 
    Plus 
    | Multiply 
    deriving (Show, Eq)

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

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

-- combine all the starting tokens that are digits into a number, also returning remaining tokens
parseDigits :: Num a => a -> [Token a] -> (a, [Token a])
parseDigits prev ((Digit value) : remaining) = parseDigits (10 * prev + value) remaining
parseDigits prev remaining = (prev, remaining)

-- parse an expression, given the expression up until this point
-- returns the remaining tokens after parsing the expression
parseExpression' :: (Eq a, Num a) => Maybe (Expression a) -> [Token a] -> (Expression a, [Token a])
parseExpression' (Just expr) [] = (expr, [])
parseExpression' Nothing (Digit value : remaining) =
    let (number, remainingTokens) = parseDigits value remaining in
        parseExpression' (Just $ Value number) remainingTokens
-- todo: handle operator precedence, if there was an operator with higher precedence previously then we should end the expression here and return to it
parseExpression' (Just leftExpr) (Operator op : remaining) = 
    let (rightExpr, remainingTokens) = parseExpression' Nothing remaining in
        parseExpression' (Just $ Expression leftExpr op rightExpr) remainingTokens
-- parse the expression in the brackets, use the CloseParen to mark the end of the expression
parseExpression' Nothing (OpenParen : remaining) = parseExpression' Nothing remaining
parseExpression' (Just expr) (CloseParen : remaining) = (expr, remaining)
-- report incorrectly formed expressions
parseExpression' Nothing (Operator _ : _) = error "no previous expression for the operator"
parseExpression' (Just _) (Digit _ : _) = error "digit shouldn't have a previous expression"
parseExpression' (Just _) (OpenParen : _) = error "open parenthesis shouldn't have a previous expression"
parseExpression' Nothing (CloseParen : _) = error "close parenthesis is not matched with an open parenthesis"
parseExpression' Nothing [] = error "empty expression"
-- only operator needs the previous expression

parseExpression :: (Eq a, Num a) => [Token a] -> Expression a
parseExpression = fst . parseExpression' Nothing

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

calculatorMain :: IO ()
calculatorMain = do
    print $ "Expression: " ++ expression
    print lexed
    print (fmap parseExpression lexed)
    print (eval expression :: Maybe Int)
    where
        expression = "11+3*(2+1)+2" -- (11+3*(2+1)+2)+100
        lexed = lexExpression expression :: Maybe [Token Int]
