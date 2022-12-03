import Data.Char
import Data.List

halfStrings :: String -> (String, String)
halfStrings s = (a, b) where
    a = take ((length s) `div` 2) s
    b = drop ((length s) `div` 2) s 

findMatch :: (String, String) -> Char
findMatch p = head [c | x <- (fst p), c <- (snd p), c == x] 

findMatch3 :: (String, String, String) -> Char
findMatch3 t = head [c | x <- (trip1 t), y <- (trip2 t), c <- (trip3 t), (c == x) && (c == y)]

trip1 :: (a, a, a) -> a
trip1 (x, _, _) = x

trip2 :: (a, a, a) -> a
trip2 (_, x, _) = x

trip3 :: (a, a, a) -> a
trip3 (_, _, x) = x

charScore :: Char -> Int
charScore c 
     | isLower c = head [x + 1 | x <- [0..25], s!!x == c]
     | isUpper c = head [x + 27 | x <- [0..25], sUp!!x == c]
     | otherwise = 0
     where s = ['a'..'z']
           sUp = ['A'..'Z']

take3 :: [a] -> (a,a,a)
take3 xs = (x,y,z) where
    x = head xs
    y = head (drop 1 xs)
    z = head (drop 2 xs)

take3s :: [a] -> [(a,a,a)]
take3s [] = []
take3s s = xs where
    xs = take3 s : take3s (drop 3 s)


main = do
    input <- readFile "rucksacks.in"
    let xs = lines input
    let pairs = map halfStrings xs
    let total = sum (map charScore (map findMatch pairs))
    print total
    let groupTriples = take3s xs
    print (sum (map charScore (map findMatch3 groupTriples)))
