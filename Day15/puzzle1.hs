import Data.List
import Data.Char
import MyCoord
import qualified Data.Set as Set
import qualified Data.List.Split as Sp

--Parse input to get beacon and sensor locations
--Find range of each sensor comparing against the nearest beacon
--Find all coordinates that each sensor can "reach" that fall on the desired y height
parseBeacons :: String -> ((Coord,Int),(Coord))
parseBeacons s = (((x,y),z),(h,v)) where 
    halves = Sp.splitOn ": " s
    x = read (init $ drop 2 $ head $ drop 2 $ words $ head halves) :: Int
    h = read (init $ drop 2 $ head $ drop 4 $ words $ last halves) :: Int
    y = read (drop 2 $ head $ drop 3 $ words $ head halves) :: Int
    v = read (drop 2 $ head $ drop 5 $ words $ last halves) :: Int
    z = (fst xy) + (snd xy) 
    xy = distancesCoords (x,y) (h,v)

canSeeAtY :: Int -> ((Coord,Int),(Coord)) -> [Coord]
canSeeAtY y (b,s) = if differenceY == 0 then [] else allBetween maxWest maxEast where
    h = fst (fst b)
    v = snd (fst b)
    range = snd b
    maxWest = ((h - differenceY),y)
    maxEast = ((h + differenceY),y)
    differenceY = if (range - manhattan) >= 0 then (range - manhattan) else 0
    manhattan = snd $ distancesCoords (h,v) (h,y)

--If can't reach y, return the origin
canSeeRange :: Int -> ((Coord,Int),(Coord)) -> (Int,Int)
canSeeRange y (b,s) = if differenceY == 0 then (0,0) else (maxWest,maxEast) where
    h = fst (fst b)
    v = snd (fst b)
    range = snd b
    maxWest = h - differenceY
    maxEast = h + differenceY
    differenceY = if (range - manhattan) >= 0 then (range - manhattan) else 0
    manhattan = snd $ distancesCoords (h,v) (h,y)

canSeeWithinXandKatY :: Int -> Int -> Int -> [((Coord,Int),Coord)] -> Bool
canSeeWithinXandKatY x k y bsList = if holeRanges x k (filter (\h -> (fst h) /= (snd h)) ranges) then False else True where
    ranges = (map canSeeRangehelper bsList)
    canSeeRangehelper z = canSeeRange y z

holeRanges :: Int -> Int -> [(Int,Int)] -> Bool
holeRanges x k ranges
    | x < (head $ sort $ map fst ranges) = True
    | k > (last $ sort $ map snd ranges) = True
    | overlapAll (sort ranges) = False
    | otherwise = True

overlapAll :: [(Int, Int)] -> Bool
overlapAll [] = True
overlapAll (current:[]) = True
overlapAll (current:next:[]) = if (snd current) >= (fst next) then True else False
overlapAll (current:ranges) = if (snd current) >= (fst $ head ranges) then overlapBoth else False where
    overlapBoth = if (snd current) >= (snd $ head ranges) then overlapAll (current:(drop 1 ranges)) else overlapAll ranges

part1 :: Set.Set Coord -> Set.Set Coord -> Set.Set Coord -> Int
part1 visible blist slist = Set.size $ Set.difference (Set.difference visible blist) slist

part2 :: Coord -> Int
part2 beacon = ((fst beacon) * y_full) + (snd beacon)

y_full = 4000000
y_test = 20

findHoleBetweenxk :: Int -> [((Coord,Int),Coord)] -> Int -> Int -> Int
findHoleBetweenxk y bsList x k
    | not $ canSeeWithinXandKatY 0 y_full y bsList = y
    | otherwise = findHoleBetweenxk (y-1) bsList x k

findNotOverlap :: [(Int, Int)] -> Int
findNotOverlap [] = 0
findNotOverlap (current:[]) = (snd current) + 1
findNotOverlap (current:next:[]) = if (snd current) >= (fst next) then 0 else (snd current) + 1
findNotOverlap (current:ranges) = if (snd current) >= (fst $ head ranges) then findNotOverlapBoth else (snd current)+1 where
    findNotOverlapBoth = if (snd current) >= (snd $ head ranges) then findNotOverlap (current:(drop 1 ranges)) else findNotOverlap ranges

findHxY :: Int -> [((Coord,Int),Coord)] -> Int
findHxY y bsList = findNotOverlap (sort $ filter (\h -> (fst h) /= (snd h)) ranges)  where
    ranges = (map canSeeRangehelper bsList)
    canSeeRangehelper z = canSeeRange y z


main = do
    input <- readFile "beacons.in"
    inputTest <- readFile "beacons.test"
    let bsList = map parseBeacons $ lines input
    --let bsList = map parseBeacons $ lines inputTest
    let visible = concat $ map canSeeAtYMapped bsList where
        canSeeAtYMapped x = canSeeAtY 2000000 x
    let result = part1 (Set.fromList visible) (Set.fromList $ map snd bsList) (Set.fromList $ map fst (map fst bsList))
    let hBeacon = findHoleBetweenxk y_full bsList 0 y_full
    let result2 = findHxY hBeacon bsList
    let final = part2 (result2, hBeacon) 
    print bsList
    print final
