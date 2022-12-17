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
canSeeAtY y (b,s) = allBetween maxWest maxEast where
    h = fst (fst b)
    v = snd (fst b)
    range = snd b
    maxWest = ((h - differenceY),y)
    maxEast = ((h + differenceY),y)
    differenceY = range - manhattan
    manhattan = snd $ distancesCoords (h,v) (h,y)

part1 :: Set.Set Coord -> [((Coord,Int),(Coord))] -> Int
part1 visible bsl = sum [1 | x <- visible', not $ coordInList x bsl] where
    visible' = Set.toList visible

coordInList :: Coord -> [((Coord,Int),(Coord))] -> Bool
coordInList x bsl
    | elem x $ map snd bsl = True
    | elem x $ map fst $ map fst bsl = True
    | otherwise = False

main = do
    input <- readFile "beacons.in"
    inputTest <- readFile "beacons.test"
    let bsList = map parseBeacons $ lines input
    --let bsList = map parseBeacons $ lines inputTest
    let visible = concat $ map canSeeAtYMapped bsList where
        canSeeAtYMapped x = canSeeAtY 2000000 x
    let result = part1 (Set.fromList visible) bsList
    print result
