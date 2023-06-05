import Data.Map (Map, (!))
import Data.Map qualified as Map

-- Define a way to set up a simple compute graph with *, +, - operators
-- It supports a forward pass (evaluate the graph) and backward pass (evaluate the gradient of the entire graph with respect to the variable)
data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Negate
  | Abs
  | Signum
  | Exp
  | Log
  | Sqrt
  | Pow
  | Sin
  | Cos
  | Asin
  | Acos
  | Atan
  | Sinh
  | Cosh
  | Asinh
  | Acosh
  | Atanh
  deriving (Eq)

instance Show Operator where
  show :: Operator -> String
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Negate = "-"
  show Abs = "abs"
  show Signum = "signum"
  show Exp = "e^"
  show Log = "log"
  show Sqrt = "√"
  show Pow = "^"
  show Sin = "sin"
  show Cos = "cos"
  show Asin = "asin"
  show Acos = "acos"
  show Atan = "atan"
  show Sinh = "sinh"
  show Asinh = "asinh"
  show Acosh = "acosh"
  show Atanh = "atanh"

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

showDerivative :: (Show a, Eq a, Floating a) => Node a -> String
showDerivative = show . derivative

instance (Eq a) => Eq (Node a) where
  (==) :: Node a -> Node a -> Bool
  (==) n1 n2 = case (n1, n2) of
    (Constant num1, Constant num2) -> num1 == num2
    (Variable name1, Variable name2) -> name1 == name2
    (Node input1 op1, Node input2 op2) -> input1 == input2 && op1 == op2
    _ -> False

instance (Eq a, Num a) => Num (Node a) where
  (+) :: Node a -> Node a -> Node a
  (+) (Constant x1) (Constant x2) = Constant $ x1 + x2
  (+) (Constant 0) var = var
  (+) var (Constant 0) = var
  (+) n1 n2
    | n1 == n2 = 2 * n1
    | otherwise =
        let baseCase = Node [n1, n2] Add
         in case (n1, n2) of
              -- also need to handle case where either one of the nodes is a multiple, and the other is just the same node
              (Node [m1, node1] Multiply, Node [m2, node2] Multiply) ->
                if node1 == node2 then Node [m1 + m2, node1] Multiply else baseCase
              (Node [m1, node1] Multiply, node2) ->
                if node1 == node2 then Node [m1 + 1, node1] Multiply else baseCase
              (node1, Node [m2, node2] Multiply) ->
                if node1 == node2 then Node [m2 + 1, node2] Multiply else baseCase
              _ -> baseCase

  (-) :: Node a -> Node a -> Node a
  (-) (Constant x1) (Constant x2) = Constant $ x1 - x2
  (-) var (Constant 0) = var
  (-) n1 (Node [n2] Negate) = n1 + n2
  (-) n1 (Node [Constant c1, n2] Multiply) = n1 + Node [Constant (negate c1), n2] Multiply
  (-) n1 n2
    | n1 == n2 = Constant 0
    | otherwise =
        let baseCase = Node [n1, n2] Subtract
         in case (n1, n2) of
              -- also need to handle case where either one of the nodes is a multiple, and the other is just the same node
              (Node [m1, node1] Multiply, Node [m2, node2] Multiply) ->
                if node1 == node2 then Node [m1 - m2, node1] Multiply else baseCase
              (Node [m1, node1] Multiply, node2) ->
                if node1 == node2 then Node [m1 - 1, node1] Multiply else baseCase
              (node1, Node [m2, node2] Multiply) ->
                if node1 == node2 then Node [m2 - 1, node2] Multiply else baseCase
              _ -> baseCase

  (*) :: Node a -> Node a -> Node a
  (*) (Constant 0) _ = Constant 0
  (*) _ (Constant 0) = Constant 0
  (*) (Constant 1) var = var
  (*) var (Constant 1) = var
  -- always keep the constant as the first term, so simplifications can be done easily
  (*) (Constant x1) (Constant x2) = Constant $ x1 * x2
  (*) n1 n2@(Constant _) = n2 * n1
  -- check if both terms have constants in them
  (*) n1 n2 = case (n1, n2) of
    (Constant c1, Node [Constant c2, node2] Multiply) -> Node [Constant (c1 * c2), node2] Multiply
    (node1, node2) -> Node [node1, node2] Multiply

  negate :: Node a -> Node a
  negate (Constant x1) = Constant $ negate x1
  negate (Node [n1] Negate) = n1
  negate (Node [Constant c1, n2] Multiply) = Node [Constant (negate c1), n2] Multiply
  negate n1 = Node [n1] Negate

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

instance (Eq a, Floating a) => Floating (Node a) where
  pi :: Node a
  pi = Constant pi

  exp :: Node a -> Node a
  exp n1 = Node [n1] Exp

  log :: Node a -> Node a
  log n1 = Node [n1] Log

  sqrt :: Node a -> Node a
  sqrt n1 = Node [n1] Sqrt

  (**) :: Node a -> Node a -> Node a
  (**) n1 n2 = Node [n1, n2] Pow

  sin :: Node a -> Node a
  sin n1 = Node [n1] Sin

  cos :: Node a -> Node a
  cos n1 = Node [n1] Cos

  asin :: Node a -> Node a
  asin n1 = Node [n1] Asin

  acos :: Node a -> Node a
  acos n1 = Node [n1] Acos

  atan :: Node a -> Node a
  atan n1 = Node [n1] Atan

  sinh :: Node a -> Node a
  sinh n1 = Node [n1] Sinh

  cosh :: Node a -> Node a
  cosh n1 = Node [n1] Cosh

  asinh :: Node a -> Node a
  asinh n1 = Node [n1] Asinh

  acosh :: Node a -> Node a
  acosh n1 = Node [n1] Acosh

  atanh :: Node a -> Node a
  atanh n1 = Node [n1] Atanh

evalNode :: Floating a => Node a -> Map String a -> a
evalNode (Constant num) _ = num
evalNode (Variable name) varMap = varMap ! name
evalNode (Node inputNodes operator) varMap =
  let args = map (`evalNode` varMap) inputNodes
   in case (args, operator) of
        ([x1, x2], Add) -> x1 + x2
        ([x1, x2], Subtract) -> x1 - x2
        ([x1, x2], Multiply) -> x1 * x2
        ([x1, x2], Divide) -> x1 / x2
        ([x], Negate) -> negate x
        ([x], Exp) -> exp x
        ([x], Log) -> log x
        ([x], Sqrt) -> sqrt x
        ([x1, x2], Pow) -> x1 ** x2
        ([x], Sin) -> sin x
        ([x], Cos) -> cos x
        ([x], Asin) -> asin x
        ([x], Acos) -> acos x
        ([x], Atan) -> atan x
        ([x], Sinh) -> sinh x
        ([x], Cosh) -> cosh x
        ([x], Asinh) -> asinh x
        ([x], Acosh) -> acosh x
        ([x], Atanh) -> atanh x
        ([x], Abs) -> abs x
        ([x], Signum) -> signum x

derivative :: (Eq a, Floating a) => Node a -> Node a
derivative (Constant _) = Constant 0
derivative (Variable _) = Constant 1
derivative (Node inputNodes operator) =
  case (inputNodes, operator) of
    ([x1, x2], Add) -> derivative x1 + derivative x2
    ([x1, x2], Subtract) -> derivative x1 - derivative x2
    ([x1, x2], Multiply) -> (x1 * derivative x2) + (derivative x1 * x2)
    ([x1, x2], Divide) -> ((x2 * derivative x1) - (x1 * derivative x2)) / (x2 * x2)
    ([x], Negate) -> negate $ derivative x
    ([x], Exp) -> derivative x * exp x
    ([x], Log) -> derivative x / x
    ([x], Sqrt) -> derivative x / (2 * sqrt x)
    ([x1, x2@(Constant _)], Pow) -> x2 * (x1 ** (x2 - 1))
    ([x1, x2], Pow) -> derivative (log x1 * x2) * (x1 ** x2) -- i.e. derivative of e^(log x1 * x2), just expressed nicer (with x1 ** x2 at the end)
    ([x], Sin) -> derivative x * cos x
    ([x], Cos) -> derivative x * negate (sin x)
    -- TODO: implement these
    ([x], Asin) -> error "Not Implemented"
    ([x], Acos) -> error "Not Implemented"
    ([x], Atan) -> error "Not Implemented"
    ([x], Sinh) -> error "Not Implemented"
    ([x], Cosh) -> error "Not Implemented"
    ([x], Asinh) -> error "Not Implemented"
    ([x], Acosh) -> error "Not Implemented"
    ([x], Atanh) -> error "Not Implemented"
    -- TODO: implement step functions as an operator, so we can define the derivative of abs and signum
    ([x], Abs) -> error "Not Implemented"
    ([x], Signum) -> error "Not Implemented"

evalInputs :: Floating a => Node a -> String -> [a] -> [a]
evalInputs expr varName = map (evalNode expr . Map.singleton varName)

main = do
  let var = Variable "x"
  let expr = 4 * (var + 5) * var
  print expr
  putStrLn $ showDerivative expr
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

  -- check double negatives are handled well
  let expr1 = negate (negate (5 * var)) - (negate var) + (negate (negate 2 * var))
  print expr1
  putStrLn $ showDerivative expr1

  -- check that multiplication can be simplified
  let expr2 = (negate 2 * (var ** 2) * 2) + (5 * var ** 2) + (var ** 2) + ((4 * var ** 3) - (var ** 3))
  print expr2
  putStrLn $ showDerivative expr2

  -- differentiate a fairly complicated function
  let expr3 = var * var / (1 + var)
  print expr3
  putStrLn $ showDerivative expr3

  -- differentiate floating function
  let expr4 = exp (cos (var ** 2))
  print expr4
  putStrLn $ showDerivative expr4
  print (evalNode expr4 (Map.singleton "x" $ sqrt pi), exp (-1))
