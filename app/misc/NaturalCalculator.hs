-- A calculator for natural expressions
module NaturalCalculator where
import Data.Maybe (mapMaybe)

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

data ContinueInstruction = Continue | Stop

-- combine all the starting tokens that are digits into a number, also returning remaining tokens
parseDigits :: Num a => a -> [Token a] -> (a, [Token a])
parseDigits prev ((Digit value) : remaining) = parseDigits (10 * prev + value) remaining
parseDigits prev remaining = (prev, remaining)

-- parse an expression, given the expression up until this point
-- returns the remaining tokens after parsing the expression
-- todo: Make this return an `Either`
parseExpression' :: (Eq a, Num a) => Maybe (Expression a) -> Bool -> [Token a] -> (Expression a, [Token a], ContinueInstruction)
parseExpression' (Just expr) _ [] = (expr, [], Stop)
parseExpression' Nothing highPrecedence (Digit value : remaining) =
    let (number, remainingTokens) = parseDigits value remaining in
        if highPrecedence -- if highPrecedence then return immediately and let the expression with higher precedence evaluate the remaining tokens
            then (Value number, remainingTokens, Continue)
            else parseExpression' (Just $ Value number) highPrecedence remainingTokens
parseExpression' (Just leftExpr) _ (Operator op : remaining) =
    let (rightExpr, remainingTokens, continueInstruction) = parseExpression' Nothing (op == Multiply) remaining
        operatorExpr = Expression leftExpr op rightExpr in
        case continueInstruction of -- stop parsing the next tokens when encountering the close bracket
            Stop -> (operatorExpr, remainingTokens, Stop) -- pass the stop instruction until getting to the open paren which will realise we're going out of a sub-xpression
            Continue -> parseExpression' (Just operatorExpr) False remainingTokens
-- reset precedence inside brackets, it's a separate operation
-- also, tell the consumer of this bracketed expression to continue parsing, after the close paren told the inner sub-expression to stop parsing
parseExpression' Nothing _ (OpenParen : remaining) = 
    let (expr, remainingTokens, _) = parseExpression' Nothing False remaining in
        (expr, remainingTokens, Continue)
parseExpression' (Just expr) _ (CloseParen : remaining) = (expr, remaining, Stop)
-- report incorrectly formed expressions
parseExpression' Nothing _ (Operator _ : _) = error "no previous expression for the operator"
parseExpression' (Just _) _ (Digit _ : _) = error "digit shouldn't have a previous expression"
parseExpression' (Just _) _ (OpenParen : _) = error "open parenthesis shouldn't have a previous expression"
parseExpression' Nothing _ (CloseParen : _) = error "close parenthesis is not matched with an open parenthesis"
parseExpression' Nothing _ [] = error "empty expression"

handleParseExpression :: (Eq a, Num a) => (Expression a, [Token a], ContinueInstruction) -> Either String (Expression a)
handleParseExpression result = case result of
    (expr, [], _) -> Right expr
    -- handle when there are leftover tokens, which can happen if there is a top level bracket like (1)+1
    (expr, remaining, Continue) -> handleParseExpression $ parseExpression' (Just expr) False remaining
    _ -> Left "Unknown error"

parseExpression :: (Show a, Eq a, Num a) => [Token a] -> Either String (Expression a)
parseExpression = handleParseExpression . parseExpression' Nothing False

-- evaluate an operation on its arguments
evalOp :: Num a => Op -> a -> a -> a
evalOp Plus = (+)
evalOp Multiply = (*)

-- evaluate an expression
evalExpression :: Num a => Expression a -> a
evalExpression (Value result) = result
evalExpression (Expression left op right) = evalOp op (evalExpression left) (evalExpression right)

-- parse and evaluate a string expression
eval :: (Show a, Eq a, Num a) => String -> Either String a
eval expression = case lexExpression expression of
        Nothing -> Left "Invalid expression formatting"
        Just tokens -> evalExpression <$> parseExpression tokens

testCases :: [(String, Int)]
testCases = [("1+3*2", 7),
    ("3*2+1", 7),
    ("1+3*2+1",8),
    ("11+3*(2+1)+2",22),
    ("1+3*(1+3*2)",22),
    ("1+3*(1+3*2)+1",23),
    ("1+3*(1+3*2+1)+1",26),
    ("0+(11+3*(2+1+1)+2)+100",125),
    ("(1)+1", 2)]

testPasses :: (String, Int) -> Bool
testPasses (exprStr, result) = eval exprStr == Right result

getFailedTest :: (String, Int) -> Maybe String
getFailedTest (exprStr, result)
    | testPasses (exprStr, result) = Nothing
    | otherwise = Just $ show $ fmap parseExpression (lexExpression exprStr :: Maybe [Token Int])

calculatorMain :: IO ()
calculatorMain = do
    print $ mapMaybe getFailedTest testCases
    print $ "Expression: " ++ expression
    print lexed
    print (fmap parseExpression lexed)
    print (eval expression :: Either String Int)
    where
        expression = "(11+3*(2+1+1)+2)+100"
        lexed = lexExpression expression :: Maybe [Token Int]
