import Data.Char
import Data.List

size_tree = 99

viewingDl :: Int -> [Int] -> Int -> Int -> Int
viewingDl 0 _ _ _ = 0
viewingDl x _ (-1) _ = x
viewingDl x xs y d = if (xs!!x) <= (xs!!y) then d else viewingDl x xs (y-1) (d+1)

viewingDr :: Int -> [Int] -> Int -> Int -> Int
viewingDr x xs y d = (viewingDl x (reverse xs) y d)

viewingDu :: Int -> [[Int]] -> Int -> Int -> Int -> Int
viewingDu 0 _ _ _ _ = 0
viewingDu x _ _ (-1) d = x
viewingDu x xs y z d = if ((xs!!x)!!y) <= ((xs!!z)!!y) then d else viewingDu x xs y (z-1) (d+1)

iterateV :: [[Int]] -> [[Int]]
iterateV xs = intsSplit [(viewingDu x xs y (x-1) 1) | x <- [0..((length xs)-1)], y <- [0..((length (xs!!x))-1)]]

iterateVdown :: [[Int]] -> [[Int]]
iterateVdown xs = reverse (intsSplit [(viewingDu x xs y (x-1) 1) | x <- [0..((length xs)-1)], y <- [0..((length (xs!!x))-1)]])

iteratel :: [[Int]] -> [[Int]] 
iteratel xs = intsSplit [(viewingDl x (xs!!y) (x-1) 1) | y <- [0..((length xs)-1)], x <- [0..((length (xs!!y))-1)]]

iterater :: [[Int]] -> [[Int]] 
iterater xs = intsSplit [(viewingDr x (xs!!y) (x-1) 1) | y <- [0..((length xs)-1)], x <- [0..((length (xs!!y))-1)]]

iterateLists :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [Int]
iterateLists xs ys zs as = [((xs!!x)!!y) * ((ys!!x)!!y) * ((zs!!x)!!y) * ((as!!x)!!y) | x <- [0..((length xs)-1)], y <- [0..((length (xs!!x))-1)]]
 

intsSplit :: [Int] -> [[Int]]
intsSplit [] = []
intsSplit xs = (x:intsSplit xs') where
    x = take size_tree xs
    xs' = drop size_tree xs

toDigs :: [String] -> [[Int]]
toDigs [] = []
toDigs (c:cs) = (map digitToInt c):(toDigs cs)

main = do
    input <- readFile "trees.in"
    let grid = toDigs (lines input)
    let upN = iterateV grid
    let down = iterateVdown (reverse grid)
    let leftN = iteratel grid
    let rightN = iterater grid
    let totals = iterateLists upN down leftN rightN
    let biggest = maximum totals
    print biggest
