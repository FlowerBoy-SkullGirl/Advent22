import Data.List
import Data.Char

--labeled x first, y second
type Coord = (Int,Int)

spotSouth :: Coord -> Coord
spotSouth (x,y) = (x,(y-1))

spotNorth :: Coord -> Coord
spotNorth (x,y) = (x,(y+1))

spotEast :: Coord -> Coord
spotEast (x,y) = ((x+1),y)

spotWest :: Coord -> Coord
spotWest (x,y) = ((x-1),y)

--Unicode vals
e_dest = 123
s_start = 97

findInGrid :: Char -> [String] -> [Coord]
findInGrid c grid = [(x,y) | y <- [0..((length grid)-1)], x <- [0..((length (grid!!y))-1)], (grid!!y)!!x == c] 

--Convert 'E' to be one value above z
gridToInt :: String -> [Int]
gridToInt s = [if x == 'E' then e_dest else (if x == 'S' then s_start else ord x) | x <- s] 

canReachEinSteps :: [[Int]] -> Coord -> Coord -> [Coord] -> Int -> Int -> Int
canReachEinSteps grid currentCoord dest_e previousCoords steps results
    | results /= 0 && (steps > results) = results
    | ((grid!!y)!!x) == 123 = steps
    | null possibleNext = results
    | otherwise = minimum $ ifNonZero $ map canReachHelper possibleNext
    where possibleNext = possiblePaths currentCoord dest_e previousCoords grid
          canReachHelper a = canReachEinSteps grid a dest_e (currentCoord:previousCoords) (steps+1) results
          x = fst currentCoord
          y = snd currentCoord
--          canReachHelper a
--            | (length a) == 1 = canReachEinSteps grid (a!!0) (currentCoord:previousCoords) (steps+1) results
--            | (length a) > 1 = canReachHelper (tail a) 
          ifNonZero xs = if null xs' then xs else xs' where xs' = [x | x <- xs, x/=0]

possiblePaths :: Coord -> Coord -> [Coord] -> [[Int]] -> [Coord]
possiblePaths currentCoord dest_e previousCoords grid = [x | x <- directions, 
                                                   (fst x) >= 0 && (snd x) >= 0
                                                   && (snd x) < (length grid)
                                                   && (fst x) < (length $ grid!!(snd x))
                                                   && nth x
                                                   && notAnyPrevious x previousCoords]
                                                  -- && ntl x]
    where directions = [(spotEast currentCoord), (spotNorth currentCoord), (spotSouth currentCoord), (spotWest currentCoord)]
          directions' = orderDistance directions dest_e
          nth x = notTooHigh currentCoord x grid
         -- ntl x = notTooLow currentCoord x grid
          notAnyPrevious x ys = if null [y | y <- ys, y == x] then True else False

--To cut down runtime.. assume that going down is invalid
notTooLow :: Coord -> Coord -> [[Int]] -> Bool
notTooLow a b grid = if h_b < h_a then False else True where
    h_a = (grid!!(snd a))!!(fst a)
    h_b = (grid!!(snd b))!!(fst b)

notTooHigh :: Coord -> Coord -> [[Int]] -> Bool
notTooHigh a b grid = if h_b > (h_a + 1) then False else True where
    h_a = (grid!!(snd a))!!(fst a)
    h_b = (grid!!(snd b))!!(fst b)

orderDistance :: [Coord] -> Coord -> [Coord]
orderDistance routes dest = map snd $ sort $ zipCoords distancesCoords dest routes

zipCoords :: (Coord -> Coord -> (Int,Int)) -> Coord -> [Coord] -> [(Int,Coord)]
zipCoords f dest [] = []
zipCoords f dest routes = ((distanceCoords $ f dest (head routes)),(head routes)):(zipCoords f dest (tail routes))

distancesCoords :: Coord -> Coord -> (Int, Int)
distancesCoords x y = ((abs ((fst x)-(fst y))),(abs ((snd x)-(snd y))))

distanceCoords :: (Int, Int) -> Int
distanceCoords (a,b) = a + b



main = do
    inputTest <- readFile "topograph.test"
    input <- readFile "topograph.map"
    --Decide which input to use
    --let grid = lines input
    let grid = lines inputTest
    --find starting pos S
    let startPos = head $ findInGrid 'S' grid 
    let dest_e = head $ findInGrid 'E' grid
    --Convert the grid to height values in int
    let intGrid = map gridToInt grid
    --Find all paths
    let results = canReachEinSteps intGrid startPos dest_e (startPos:[]) 0 0
    --Remove any failed paths from results and find shortest
    print intGrid
    print results
