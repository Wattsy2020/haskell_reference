module Homework4 where

import Data.Set qualified as Set
import Board
import VectorUtils qualified as Vec
import Data.Maybe (mapMaybe)

isGraphClosed :: forall a. Ord a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed startNode getAdjNodes predicate = fst $ isClosed' startNode Set.empty
    where
        isClosedFold' :: Ord a => (Bool, Set.Set a) -> a -> (Bool, Set.Set a)
        isClosedFold' (False, visited) _ = (False, visited)
        isClosedFold' (True, visited) node = isClosed' node visited

        isClosed' :: Ord a => a -> Set.Set a -> (Bool, Set.Set a)
        isClosed' node visited
            | Set.member node visited = (True, visited)
            | predicate node = foldl isClosedFold' (True, newVisited) $ getAdjNodes node
            | otherwise = (False, newVisited)
                where
                    newVisited = Set.insert node visited

data Cell a = Cell Int Int a deriving (Eq, Ord) -- store data and its position in the Vector

-- have to wrap the vector elements into Cells so we can implement getAdjNodes
isClosed :: MazeGrid -> Bool
isClosed grid = case startNode of
        Nothing -> False
        Just node -> isGraphClosed node getAdjacent predicate
    where
        gridWithCells = Vec.imapGrid Cell grid
        startNode = Vec.indexGrid 1 1 gridWithCells

        predicate :: Cell Tile -> Bool
        predicate (Cell x y _) = x > 0 && x < 9 && y < 9 -- check the bounds of the maze

        isWall :: Cell Tile -> Bool
        isWall (Cell _ _ Wall) = True
        isWall _ = False

        getAdjacent :: Cell Tile -> [Cell Tile]
        getAdjacent (Cell row col _) =
            let adjCoords = [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)] in
                filter (not . isWall) $ mapMaybe (\(r, c) -> Vec.indexGrid r c gridWithCells) adjCoords

homework4Main :: IO ()
homework4Main = print $ isClosed (grid maze)
