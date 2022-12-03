import Data.List
-- A and X are Rock: 1 point
-- B and Y are paper: 2 points
-- C and Z are scissors: 3 points
-- Lose 0, draw 3, win 6
-- X Lose, Y draw, Z win 

lineToPairs :: String -> (Char, Char)
lineToPairs s = (a, b) where
  a = head s
  b = last s

--puzzle2
pairCompare2 :: (Char, Char) -> Int
pairCompare2 a
    | snd a == 'X' = case fst a of 'A' -> 3
                                   'B' -> 1
                                   'C' -> 2
    | snd a == 'Y' = case fst a of 'A' -> 4
                                   'B' -> 5
                                   'C' -> 6
    | snd a == 'Z' = case fst a of 'A' -> 8
                                   'B' -> 9
                                   'C' -> 7
    | otherwise = 0

--puzzle1
pairCompare :: (Char, Char) -> Int
pairCompare a
    | snd a == 'X' = case fst a of 'A' -> 4
                                   'B' -> 1
                                   'C' -> 7
    | snd a == 'Y' = case fst a of 'A' -> 8
                                   'B' -> 5
                                   'C' -> 2
    | snd a == 'Z' = case fst a of 'A' -> 3
                                   'B' -> 9
                                   'C' -> 6
    | otherwise = 0

--goal String -> [String] -> [(Char, Char)] -> [Int] -> Int
--     readfile  lines       parse             compare  sum
--main :: IO()
main = do
    input <- readFile "rps.in"
    let xs = lines input
    let pairs = map lineToPairs xs
    let points = sum (map pairCompare pairs)
    let points2 = sum (map pairCompare2 pairs)
    print points
    print points2
