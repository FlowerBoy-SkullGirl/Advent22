import Data.List
import Data.Char

--id, list of items, worry operation, test, monkeypair, inspectionCount
type Monkey = (Integer, [Integer], (Char,Integer), Integer, (Integer,Integer), Integer)

idM :: Monkey -> Integer
idM (a,_,_,_,_,_) = a

itemsM :: Monkey -> [Integer]
itemsM (_,a,_,_,_,_) = a

operM :: Monkey -> (Char,Integer)
operM (_,_,a,_,_,_) = a

testM :: Monkey -> Integer
testM (_,_,_,a,_,_) = a

mlM :: Monkey -> (Integer,Integer)
mlM (_,_,_,_,a,_) = a

countM :: Monkey -> Integer
countM (_,_,_,_,_,a) = a

parseLines :: [String] -> [Monkey]
parseLines [] = []
parseLines (s:a:r:t:d:g:[]) = (makeMonkey (s:a:r:t:d:g:[])):[]
parseLines s = (makeMonkey (take 6 s)):(parseLines (drop 7 s))

makeMonkey :: [String] -> Monkey
makeMonkey s = ((getID (s!!0)),(getItems (s!!1)),(getOps (s!!2)),(getTest (s!!3)),((getM (s!!4)),(getM (s!!5))),0)

getID :: String -> Integer
getID s = toInteger $ digitToInt $ last $ init $ s

getItems :: String -> [Integer]
getItems s = map toInteger $ map read (dropComm $ filter (\h -> (length h) <= 3) $ words s) where
    dropComm s = map (\n -> if (length n) == 3 then init n else n) s

--(-1 to signify 'old')
getOps :: String -> (Char,Integer)
getOps s = ((head $ last $ init s'),(oldp s')) where
    s' = words s
    oldp x = if not $ isDigit $ head $ last x then (-1) else read (last x) :: Integer


getTest :: String -> Integer
getTest s = read (last $ words s) :: Integer

getM :: String -> Integer
getM s = read (last $ words s) :: Integer

monkeyTest :: Integer -> Integer -> Bool
monkeyTest x y = if x `mod` y == 0 then True else False

--Negative one will be 'old,' x
monkeyOperation :: Integer -> Integer -> (Char,Integer) -> Integer
monkeyOperation x lcmM (c,y) 
    | x > lcmM = monkeyOperation x' lcmM (c,y)
    | y == (-1) = monkeyOperation x lcmM (c,x)
    | c == '*' = x * y
    | c == '+' = x + y
    --where x' = until (<= lcmM) (subtract lcmM) x
    where x' = x `mod` lcmM

monkeyDecision :: Bool -> (Integer,Integer) -> Integer
monkeyDecision b (x,y) = if b then x else y

doMonkeyOperation :: Integer -> Monkey -> Monkey
doMonkeyOperation lcmM m = ((idM m),items',(operM m),(testM m),(mlM m),((countM m)+(toInteger (length $ itemsM m)))) where
    items' = map helperOp (itemsM m)
    helperOp x = monkeyOperation x lcmM (operM m)

doMonkeyTest :: Monkey -> (Monkey,([Bool],[Integer]))
doMonkeyTest m = (((idM m),items',(operM m),(testM m),(mlM m),(countM m)),bl) where
    items' = []
    bl = ((map helperTest (itemsM m)),(itemsM m))
    helperTest x = monkeyTest x (testM m)

--Takes a list of items and there test outcome, decides which monkey id to send items to, and adds those items so the monkey's list
--Abandon hope all ye who enter here
doMonkeyDecision :: [Monkey] -> (Monkey,([Bool],[Integer])) -> [Monkey]
doMonkeyDecision ms (currM,bl) = [if elem (toInteger x) decisions then m' x else (if isCur x then currM else ms!!x) | x <- [0..((length ms)-1)]] where
    isCur x = (idM (ms!!x)) == (idM currM)
    decisions = [if ((fst bl)!!z) then fst $ mlM currM else snd $ mlM currM | z <- [0..((length $ fst bl)-1)]]
    m' x = (idM (ms!!x),items',operM (ms!!x),testM (ms!!x),mlM (ms!!x), countM (ms!!x))
        where items'= (itemsM (ms!!x)) ++ [(snd bl)!!y | y <- [0..((length $ fst bl)-1)], (decisions!!y) == idM (ms!!x)]

doMonkeyTurn :: Integer -> [Monkey] -> Int -> [Monkey]
doMonkeyTurn lcmM ms x 
    | null $ itemsM currentM = ms
    | otherwise = doMonkeyDecision ms $ doMonkeyTest $ doMonkeyOperation lcmM currentM
    where currentM = ms!!x

doxMTs :: Integer -> [Monkey] -> Int -> [Monkey]
doxMTs lcmM ms x 
    | x == 0 = doMonkeyTurn lcmM ms (xs!!x)
    | otherwise = doxMTs lcmM ms' (x-1)
    where xs = [((length ms)-1),((length ms)-2)..0]
          ms' = doMonkeyTurn lcmM ms (xs!!x)

doxMRs :: Integer -> [Monkey] -> Int -> [Monkey]
doxMRs lcmM ms x
    | x == 1 = doxMTs lcmM ms ((length ms)-1)
    | otherwise = doxMRs lcmM ms' (x-1)
    where ms' = doxMTs lcmM ms ((length ms)-1)

findMonkeyBusiness :: [Monkey] -> Integer
findMonkeyBusiness ms = x * y where
    x = countM $ head ms'
    y = countM $ last ms'
    ms' = findMostActiveMonkeys ms

findMostActiveMonkeys :: [Monkey] -> [Monkey]
findMostActiveMonkeys ms = take 2 $ reverse $ sortMonkeys ms

sortMonkeys :: [Monkey] -> [Monkey]
sortMonkeys ms = [x | y <- (sort $ zipCount ms), x <- ms, (idM x) == (snd y)]

zipCount :: [Monkey] -> [(Integer,Integer)]
zipCount ms = zip [countM x | x <- ms] [idM x | x <- ms]

monkeyLCM :: [Monkey] -> Integer
monkeyLCM [] = 1
monkeyLCM ms = lcm (testM $ head ms) (monkeyLCM (tail ms))

main = do
    input <- readFile "monkey.business"
    inputTest <- readFile "monkey.test"
    --let monkeylist = parseLines $ lines inputTest
    let monkeylist = parseLines $ lines input
    let lcmM = monkeyLCM monkeylist
    let monkeyRound = doxMRs lcmM monkeylist 10000
    let monkeyBusiness = findMonkeyBusiness monkeyRound
    print monkeylist
    print monkeyRound
    print monkeyBusiness
