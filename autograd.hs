import Data.Map (Map, (!))
import Data.Map qualified as Map

-- Define a way to set up a simple compute graph with *, +, - operators
-- It supports a forward pass (evaluate the graph) and backward pass (evaluate the gradient of the entire graph with respect to the variable)
data Operator = Add | Subtract | Multiply | Divide | Abs | Signum deriving (Eq)

instance Show Operator where
  show :: Operator -> String
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Abs = "abs"
  show Signum = "signum"

data Node a
  = Node {inputNodes :: [Node a], operator :: Operator}
  | Constant {value :: a}
  | Variable {name :: String}

instance Show a => Show (Node a) where
  show :: Node a -> String
  show (Constant num) = show num
  show (Variable string) = string
  show (Node inputNodes operator) =
    case inputNodes of
      [x1, x2] -> "(" ++ show x1 ++ " " ++ show operator ++ " " ++ show x2 ++ ")"
      [x] -> "(" ++ show operator ++ " " ++ show x ++ ")"

showDerivative :: (Show a, Eq a, Fractional a) => Node a -> String
showDerivative = show . derivative

instance (Eq a) => Eq (Node a) where
  (==) :: Node a -> Node a -> Bool
  (==) n1 n2 = case (n1, n2) of
    (Constant num1, Constant num2) -> num1 == num2
    (Variable name1, Variable name2) -> name1 == name2
    (Node input1 op1, Node input2 op2) -> input1 == input2 && op1 == op2

instance (Eq a, Num a) => Num (Node a) where
  (+) :: Node a -> Node a -> Node a
  (+) (Constant x1) (Constant x2) = Constant $ x1 + x2
  (+) (Constant 0) var = var
  (+) var (Constant 0) = var
  (+) n1@(Variable name1) n2@(Variable name2)
    | name1 == name2 = 2 * n1
    | otherwise = Node [n1, n2] Add
  (+) n1 n2 = Node [n1, n2] Add

  (-) :: Node a -> Node a -> Node a
  (-) (Constant x1) (Constant x2) = Constant $ x1 - x2
  (-) var (Constant 0) = var
  (-) n1@(Variable name1) n2@(Variable name2)
    | name1 == name2 = Constant 0
    | otherwise = Node [n1, n2] Subtract
  (-) n1 n2 = Node [n1, n2] Subtract

  (*) :: Node a -> Node a -> Node a
  (*) (Constant x1) (Constant x2) = Constant $ x1 * x2
  (*) (Constant 0) _ = Constant 0
  (*) _ (Constant 0) = Constant 0
  (*) (Constant 1) var = var
  (*) var (Constant 1) = var
  (*) n1 n2 = Node [n1, n2] Multiply

  abs :: Node a -> Node a
  abs n1 = Node [n1] Abs

  signum :: Node a -> Node a
  signum n1 = Node [n1] Signum

  fromInteger :: Integer -> Node a
  fromInteger = Constant . fromInteger

instance (Eq a, Fractional a) => Fractional (Node a) where
  (/) :: Node a -> Node a -> Node a
  (/) (Constant x1) (Constant x2) = Constant $ x1 / x2
  (/) (Constant 0) _ = Constant 0
  (/) var (Constant 1) = var
  (/) n1 n2 = Node [n1, n2] Divide

  fromRational :: Rational -> Node a
  fromRational = Constant . fromRational

evalNode :: Fractional a => Node a -> Map String a -> a
evalNode (Constant num) _ = num
evalNode (Variable name) varMap = varMap ! name
evalNode (Node inputNodes operator) varMap =
  let args = map (`evalNode` varMap) inputNodes
   in case (args, operator) of
        ([x1, x2], Add) -> x1 + x2
        ([x1, x2], Subtract) -> x1 - x2
        ([x1, x2], Multiply) -> x1 * x2
        ([x1, x2], Divide) -> x1 / x2
        ([x], Abs) -> abs x
        ([x], Signum) -> signum x

derivative :: (Eq a, Fractional a) => Node a -> Node a
derivative (Constant _) = Constant 0
derivative (Variable _) = Constant 1
derivative (Node inputNodes operator) =
  case (inputNodes, operator) of
    ([x1, x2], Add) -> derivative x1 + derivative x2
    ([x1, x2], Subtract) -> derivative x1 - derivative x2
    ([x1, x2], Multiply) -> (x1 * derivative x2) + (derivative x1 * x2)
    ([x1, x2], Divide) -> ((x2 * derivative x1) - (x1 * derivative x2)) / (x2 * x2)
    -- TODO: implement step functions as an operator, so we can define the derivative of abs and signum
    ([x], Abs) -> error "Not Implemented"
    ([x], Signum) -> error "Not Implemented"

evalInputs :: Fractional a => Node a -> String -> [a] -> [a]
evalInputs expr varName = map (evalNode expr . Map.singleton varName)

main = do
  let var = Variable "x"
  let expr = 4 * (var + 5) * var
  print expr
  print $ showDerivative expr
  let varMap = Map.singleton "x" 5
  print $ evalNode expr varMap
  print $ evalNode (derivative expr) varMap

  -- plot a parabola
  let parabola = var * var
  let xs = [-4 .. 4]
  let ys = evalInputs parabola "x" xs
  let dydxs = evalInputs (derivative parabola) "x" xs
  print (parabola, showDerivative parabola)
  print $ zip3 xs ys dydxs

  -- differentiate a fairly complicated function
  let expr = var * var / (1 + var)
  print expr
  print $ showDerivative expr
