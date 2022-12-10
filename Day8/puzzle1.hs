import Data.Char
import Data.List

size_tree = 99

maskLeft :: String -> [Int]
maskLeft s = [if (gtall x s (x-1)) then 1 else 0 | x <- [0..((length s)-1)]]

gtall :: Int -> String -> Int -> Bool
gtall 0 _ _ = True
gtall _ _ (-1) = True
gtall x s y = if (digitToInt (s!!y)) >= (digitToInt (s!!x)) then False else gtall x s (y-1)

maskRight :: String -> [Int]
maskRight s = reverse (maskLeft (reverse s))

maskTop :: [String] -> [Int]
maskTop grid = [if (gtallVert x y grid (x-1)) then 1 else 0 | x <- [0..((length grid)-1)], y <- [0..((length (grid!!x))-1)]]

gtallVert :: Int -> Int -> [String] -> Int -> Bool
gtallVert _ 0 _ _ = True
gtallVert 0 _ _ _ = True
gtallVert _ _ _ (-1) = True
gtallVert x y grid z = if (digitToInt ((grid!!x)!!y)) <= (digitToInt ((grid!!z)!!y)) 
                           then False
                           else gtallVert x y grid (z-1)

maskBottom :: [String] -> [Int]
maskBottom grid = [if (gtallVert' x y grid (x+1)) then 1 else 0 | x <- [0..((length grid)-1)], y <- [0..((length (grid!!x))-1)]]

gtallVert' :: Int -> Int -> [String] -> Int -> Bool
gtallVert' _ 0 _ _ = True
gtallVert' 0 _ _ _ = True
gtallVert' _ _ _ 99 = True
gtallVert' x y grid z = if (digitToInt ((grid!!x)!!y)) <= (digitToInt ((grid!!z)!!y)) 
                           then False
                           else gtallVert' x y grid (z+1)

intsSplit :: [Int] -> [[Int]]
intsSplit [] = []
intsSplit xs = (x:intsSplit xs') where
    x = take size_tree xs
    xs' = drop size_tree xs

anyOf :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
anyOf xs ys zs as = intsSplit [if (((xs!!n)!!m) == 1) || (((ys!!n)!!m) == 1) || (((zs!!n)!!m) == 1) || (((as!!n)!!m) == 1) then 1 else 0 | n <- [0..((length xs)-1)], m <- [0..((length (xs!!n))-1)]]

main = do
    input <- readFile "trees.in"
    let grid = lines input
    let fromLeft = map maskLeft grid
    let fromRight = map maskRight grid
    let fromTop = intsSplit (maskTop grid)
    let fromBottom = intsSplit (maskBottom grid)
    let maskOf = anyOf fromLeft fromRight fromTop fromBottom
    print maskOf
    print (map sum maskOf)
    print (sum (map sum maskOf))
