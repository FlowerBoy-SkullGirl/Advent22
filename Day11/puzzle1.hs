import Data.List
import Data.Char

--id, list of items, worry operation, test, monkeypair, inspectionCount
type Monkey = (Int, [Int], (Char,Int), Int, (Int,Int), Int)

idM :: Monkey -> Int
idM (a,_,_,_,_,_) = a

itemsM :: Monkey -> [Int]
itemsM (_,a,_,_,_,_) = a

operM :: Monkey -> (Char,Int)
operM (_,_,a,_,_,_) = a

testM :: Monkey -> Int
testM (_,_,_,a,_,_) = a

mlM :: Monkey -> (Int,Int)
mlM (_,_,_,_,a,_) = a

countM :: Monkey -> Int
countM (_,_,_,_,_,a) = a

parseLines :: [String] -> [Monkey]
parseLines [] = []
parseLines (s:a:r:t:d:g:[]) = (makeMonkey (s:a:r:t:d:g:[])):[]
parseLines s = (makeMonkey (take 6 s)):(parseLines (drop 7 s))

makeMonkey :: [String] -> Monkey
makeMonkey s = ((getID (s!!0)),(getItems (s!!1)),(getOps (s!!2)),(getTest (s!!3)),((getM (s!!4)),(getM (s!!5))),0)

getID :: String -> Int
getID s = digitToInt $ last $ init $ s

getItems :: String -> [Int]
getItems s = map read (dropComm $ filter (\h -> (length h) <= 3) $ words s) where
    dropComm s = map (\n -> if (length n) == 3 then init n else n) s

--(-1 to signify 'old')
getOps :: String -> (Char,Int)
getOps s = ((head $ last $ init s'),(oldp s')) where
    s' = words s
    oldp x = if not $ isDigit $ head $ last x then (-1) else read (last x) :: Int


getTest :: String -> Int
getTest s = read (last $ words s) :: Int

getM :: String -> Int
getM s = read (last $ words s) :: Int

monkeyTest :: Int -> Int -> Bool
monkeyTest x y = if x `mod` y == 0 then True else False

--Negative one will be 'old,' x
monkeyOperation :: Int -> (Char,Int) -> Int
monkeyOperation x (c,y) 
    | y == (-1) = monkeyOperation x (c,x)
    | c == '*' = (x * y) `div` 3
    | c == '+' = (x + y) `div` 3

monkeyDecision :: Bool -> (Int,Int) -> Int
monkeyDecision b (x,y) = if b then x else y

doMonkeyOperation :: Monkey -> Monkey
doMonkeyOperation m = ((idM m),items',(operM m),(testM m),(mlM m),((countM m)+(length $ itemsM m))) where
    items' = map helperOp (itemsM m)
    helperOp x = monkeyOperation x (operM m)

doMonkeyTest :: Monkey -> (Monkey,([Bool],[Int]))
doMonkeyTest m = (((idM m),items',(operM m),(testM m),(mlM m),(countM m)),bl) where
    items' = []
    bl = ((map helperTest (itemsM m)),(itemsM m))
    helperTest x = monkeyTest x (testM m)

--Takes a list of items and there test outcome, decides which monkey id to send items to, and adds those items so the monkey's list
doMonkeyDecision :: [Monkey] -> (Monkey,([Bool],[Int])) -> [Monkey]
doMonkeyDecision ms (currM,bl) = [if elem x decisions then m' x else (if isCur x then currM else ms!!x) | x <- [0..((length ms)-1)]] where
    isCur x = (idM (ms!!x)) == (idM currM)
    decisions = [if ((fst bl)!!z) then fst $ mlM currM else snd $ mlM currM | z <- [0..((length $ fst bl)-1)]]
    m' x = (idM (ms!!x),items',operM (ms!!x),testM (ms!!x),mlM (ms!!x), countM (ms!!x))
        where items'= (itemsM (ms!!x)) ++ [(snd bl)!!y | y <- [0..((length $ fst bl)-1)], (decisions!!y) == idM (ms!!x)]

doMonkeyTurn :: [Monkey] -> Int -> [Monkey]
doMonkeyTurn ms x 
    | null $ itemsM currentM = ms
    | otherwise = doMonkeyDecision ms $ doMonkeyTest $ doMonkeyOperation currentM
    where currentM = ms!!x

doxMTs :: [Monkey] -> Int -> [Monkey]
doxMTs ms x 
    | x == 0 = doMonkeyTurn ms (xs!!x)
    | otherwise = doxMTs ms' (x-1)
    where xs = [((length ms)-1),((length ms)-2)..0]
          ms' = doMonkeyTurn ms (xs!!x)

doxMRs :: [Monkey] -> Int -> [Monkey]
doxMRs ms x
    | x == 1 = doxMTs ms ((length ms)-1)
    | otherwise = doxMRs ms' (x-1)
    where ms' = doxMTs ms ((length ms)-1)

findMonkeyBusiness :: [Monkey] -> Int
findMonkeyBusiness ms = x * y where
    x = countM $ head ms'
    y = countM $ last ms'
    ms' = findMostActiveMonkeys ms

findMostActiveMonkeys :: [Monkey] -> [Monkey]
findMostActiveMonkeys ms = take 2 $ reverse $ sortMonkeys ms

sortMonkeys :: [Monkey] -> [Monkey]
sortMonkeys ms = [x | y <- (sort $ zipCount ms), x <- ms, (idM x) == (snd y)]

zipCount :: [Monkey] -> [(Int,Int)]
zipCount ms = zip [countM x | x <- ms] [idM x | x <- ms]

main = do
    input <- readFile "monkey.business"
    inputTest <- readFile "monkey.test"
    --let monkeylist = parseLines $ lines inputTest
    let monkeylist = parseLines $ lines input
    let monkeyRound = doxMRs monkeylist 20
    let monkeyBusiness = findMonkeyBusiness monkeyRound
    print monkeylist
    print monkeyRound
    print monkeyBusiness
