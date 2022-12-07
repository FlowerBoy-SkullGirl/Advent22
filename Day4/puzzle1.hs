import Data.List
import Data.Char
import Data.List.Split as Sp 

listToPair :: [String] -> (String, String)
listToPair s = (x, y) where
    x = head s
    y = last s

sToIntList :: (String, String) -> ([Int], [Int])
sToIntList (x, y) = ([a..b], [c..d]) where
    a = read (head (Sp.splitOn "-" x)) :: Int
    b = read (last (Sp.splitOn "-" x)) :: Int
    c = read (head (Sp.splitOn "-" y)) :: Int
    d = read (last (Sp.splitOn "-" y)) :: Int

pairContains :: Eq a => ([a], [a]) -> Bool
pairContains p = (isInfixOf (fst p) (snd p)) || (isInfixOf (snd p) (fst p))

findMatch :: Eq a => ([a], [a]) -> Bool
findMatch p = not (null xs) where
    xs = [c | x <- (fst p), c <- (snd p), c == x] 

boolInt :: Bool -> Int
boolInt True = 1
boolInt False = 0

main = do
    input <- readFile "sections.in"
    let groups = lines input
    let pairs = map listToPair (map splitGroups groups) where
        splitGroups = Sp.splitOn "," 
    let sections = map sToIntList pairs
    let duplicates = map pairContains sections
    let total = sum (map boolInt duplicates) 
    print total
    let total2 = sum (map boolInt (map findMatch sections))
    print total2
