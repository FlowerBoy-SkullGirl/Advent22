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

canSeeRange :: Int -> ((Coord,Int),(Coord)) -> Set.Set Int
canSeeRange y (b,s) = Set.fromAscList [maxWest..maxEast] where
    h = fst (fst b)
    v = snd (fst b)
    range = snd b
    maxWest = h - differenceY
    maxEast = h + differenceY
    differenceY = range - manhattan
    manhattan = snd $ distancesCoords (h,v) (h,y)

canSeeWithinXandKatY :: Int -> Int -> Int -> [((Coord,Int),Coord)] -> Bool
canSeeWithinXandKatY x k y bsList = if Set.isSubsetOf (Set.fromAscList [x..k]) uSets then True else False where
    uSets = unionListOfSets (map canSeeRangehelper bsList)
    canSeeRangehelper z = canSeeRange y z

unionListOfSets :: [Set.Set Int] -> Set.Set Int
unionListOfSets [] = Set.empty
unionListOfSets (last:[]) = last
unionListOfSets sl = Set.union (head sl) sl' where
    sl' = unionListOfSets (drop 1 sl)

part1 :: Set.Set Coord -> Set.Set Coord -> Set.Set Coord -> Int
part1 visible blist slist = Set.size $ Set.difference (Set.difference visible blist) slist

part2 :: Coord -> Int
part2 beacon = ((fst beacon) * y_full) + (snd beacon)

noMaybes :: Maybe Int -> Int
noMaybes Nothing = (-1)
noMaybes (Just x) = x

y_full = 4000000
y_test = 20

main = do
    input <- readFile "beacons.in"
    inputTest <- readFile "beacons.test"
    let bsList = map parseBeacons $ lines input
    --let bsList = map parseBeacons $ lines inputTest
    let visible = concat $ map canSeeAtYMapped bsList where
        canSeeAtYMapped x = canSeeAtY 2000000 x
    let result = part1 (Set.fromList visible) (Set.fromList $ map snd bsList) (Set.fromList $ map fst (map fst bsList))
    let hBeacon = noMaybes $ elemIndex False $ map canSeeHelper [0..y_full] where
        canSeeHelper x = canSeeWithinXandKatY 0 y_full x bsList
    let result2 = [(x,hBeacon) | x <- [0..y_full]] \\ (concat $ map canSeeAtYMapped bsList) where
        canSeeAtYMapped x = canSeeAtY hBeacon x
    let final = part2 (head result2)
    print bsList
    print final
