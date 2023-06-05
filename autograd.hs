import Data.Map (Map, (!))
import Data.Map qualified as Map

-- Define a way to set up a simple compute graph with *, +, - operators
-- It supports a forward pass (evaluate the graph) and backward pass (evaluate the gradient of the entire graph with respect to the variable)
data Operator = Add | Subtract | Multiply | Abs | Signum deriving (Show)

data Node a
  = Node {inputNodes :: [Node a], operator :: Operator}
  | Constant {value :: a}
  | Variable {name :: String}
  deriving (Show)

instance Num a => Num (Node a) where
  (+) :: Node a -> Node a -> Node a
  (+) n1 n2 = Node [n1, n2] Add

  (-) :: Node a -> Node a -> Node a
  (-) n1 n2 = Node [n1, n2] Subtract

  (*) :: Node a -> Node a -> Node a
  (*) n1 n2 = Node [n1, n2] Multiply

  abs :: Node a -> Node a
  abs n1 = Node [n1] Abs

  signum :: Node a -> Node a
  signum n1 = Node [n1] Signum

  fromInteger :: Integer -> Node a
  fromInteger = Constant . fromInteger

evalNode :: Num a => Node a -> Map String a -> a
evalNode (Constant num) _ = num
evalNode (Variable name) varMap = varMap ! name
evalNode (Node inputNodes operator) varMap =
  let args = map (`evalNode` varMap) inputNodes
   in case (args, operator) of
        ([x1, x2], Add) -> x1 + x2
        ([x1, x2], Subtract) -> x1 - x2
        ([x1, x2], Multiply) -> x1 * x2
        ([x], Abs) -> abs x
        ([x], Signum) -> signum x

main = do
  let var = Variable "x"
  let expr = 4 * (var + 5) * var
  print expr
  print $ evalNode expr (Map.singleton "x" 5)
  print $ evalNode expr (Map.singleton "x" 1)