quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) = quicksort smaller ++ [pivot] ++ quicksort larger
    where   smaller = filter (< pivot) xs
            larger  = filter (> pivot) xs
--main = print (quicksort [5, 4, 3, 2, 1], quicksort [1, 2, 3, 4, 5], quicksort [10, 5, 7, 8, 2, -1], quicksort "fedgabc")

-- how does it work on infinite lists? It doesn't, needs to evaluate the entire expression to the base case to take the first 10
largeNums = [0, 1..10000] 
-- largeNums = [10000, 9999..0] does particularly badly, as this is the worst case for quicksort 
main = print (take 10 largeNums, take 10 (quicksort largeNums))
