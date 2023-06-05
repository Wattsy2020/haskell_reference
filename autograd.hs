import Data.Map (Map, (!))
import Data.Map qualified as Map

-- Define a way to set up a simple compute graph with *, +, - operators
-- It supports a forward pass (evaluate the graph) and backward pass (evaluate the gradient of the entire graph with respect to the variable)
data Operator = Add | Subtract | Multiply | Abs | Signum

instance Show Operator where
  show :: Operator -> String
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Abs = "abs"
  show Signum = "signum"

data Node a
  = Node {inputNodes :: [Node a], operator :: Operator, gradient :: Node a}
  | Constant {value :: a}
  | Variable {name :: String}
  | NullDerivative -- to indicate that a derivative node has no gradient (prevent infinitely calculating the gradient recursively)

instance Show a => Show (Node a) where
  show :: Node a -> String
  show (Constant num) = show num
  show (Variable string) = string
  show (Node inputNodes operator _) =
    case inputNodes of
      [x1, x2] -> "(" ++ show x1 ++ " " ++ show operator ++ " " ++ show x2 ++ ")"
      [x] -> "(" ++ show operator ++ " " ++ show x ++ ")"

showDerivative :: Show a => Node a -> String
showDerivative = show . gradient

derivative :: Num a => Node a -> Node a
derivative (Constant _) = Constant 0
derivative (Variable _) = Constant 1
derivative (Node _ _ gradient) = gradient

instance Num a => Num (Node a) where
  (+) :: Node a -> Node a -> Node a
  (+) n1 n2 = Node [n1, n2] Add (Node [derivative n1, derivative n2] Add NullDerivative)

  (-) :: Node a -> Node a -> Node a
  (-) n1 n2 = Node [n1, n2] Subtract (Node [derivative n1, derivative n2] Subtract NullDerivative)

  (*) :: Node a -> Node a -> Node a
  (*) n1 n2 = Node [n1, n2] Multiply gradient
    where
      vu' = Node [n1, derivative n2] Multiply NullDerivative
      uv' = Node [derivative n1, n2] Multiply NullDerivative
      gradient = Node [vu', uv'] Add NullDerivative

  -- TODO: implement step functions as operators, so we can define the derivative of abs and signum
  abs :: Node a -> Node a
  abs n1 = Node [n1] Abs NullDerivative

  signum :: Node a -> Node a
  signum n1 = Node [n1] Signum NullDerivative

  fromInteger :: Integer -> Node a
  fromInteger = Constant . fromInteger

evalNode :: Num a => Node a -> Map String a -> a
evalNode (Constant num) _ = num
evalNode (Variable name) varMap = varMap ! name
evalNode (Node inputNodes operator _) varMap =
  let args = map (`evalNode` varMap) inputNodes
   in case (args, operator) of
        ([x1, x2], Add) -> x1 + x2
        ([x1, x2], Subtract) -> x1 - x2
        ([x1, x2], Multiply) -> x1 * x2
        ([x], Abs) -> abs x
        ([x], Signum) -> signum x

evalInputs :: Num a => Node a -> String -> [a] -> [a]
evalInputs expr varName = map (evalNode expr . Map.singleton varName)

main = do
  let var = Variable "x"
  let expr = 4 * (var + 5) * var
  print expr
  print $ showDerivative expr
  let varMap = Map.singleton "x" 5
  print $ evalNode expr varMap
  print $ evalNode (gradient expr) varMap

  -- plot a parabola
  let parabola = var * var
  let xs = [-4 .. 4]
  let ys = evalInputs parabola "x" xs
  let dydxs = evalInputs (gradient parabola) "x" xs
  print (parabola, showDerivative parabola)
  print $ zip3 xs ys dydxs
