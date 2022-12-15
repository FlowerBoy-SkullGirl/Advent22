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

notTooHigh :: Coord -> Coord -> [[Int]] -> Bool
notTooHigh a b grid = if h_b > (h_a + 1) then False else True where
    h_a = (grid!!(snd a))!!(fst a)
    h_b = (grid!!(snd b))!!(fst b)

--If list of prev has start pos, return number of steps from end
--if not, find all positions that are 1 step farther from the end to see if they have start
findXinGrid :: Coord -> Int -> [[Int]] -> [(Coord,Int)] -> Int
findXinGrid startPos x grid visited
     | visited `containsCoord` startPos = x
     | otherwise = findXinGrid startPos (x+1) grid visited'
     where visited' = neighboursInReach x grid visited (coordsFilterX visited x)
           containsCoord ys x = if not $ null [y | (y,z) <- ys, y == x && z >= 0] then True else False

--Find all adj positions such that they are at most one step away from the current round
neighboursInReach :: Int -> [[Int]] -> [(Coord,Int)] -> [(Coord,Int)] -> [(Coord,Int)]
neighboursInReach x grid visited visitedX
    | null visitedX = []
    | (length visitedX) == 1 = findEligibleAdj (head visitedX) grid visited
    | otherwise = findEligibleAdj (head visitedX) grid visited'
    where visited' = neighboursInReach x grid visited (drop 1 visitedX)

--Select only the previous coordinates at x steps away from start
coordsFilterX :: [(Coord,Int)] -> Int -> [(Coord,Int)]
coordsFilterX all x = [allX | allX <- all, (snd allX) == x]

--Reject adj coords that are off grid, too low to reach current coord, or already evaluated
findEligibleAdj :: (Coord,Int) -> [[Int]] -> [(Coord,Int)] -> [(Coord,Int)]
findEligibleAdj (current,cx) grid previous = [(x,(cx+1)) | x <- adj,
                                                   (fst x) >= 0 && (snd x) >= 0
                                                   && (snd x) < (length grid)
                                                   && (fst x) < (length $ grid!!(snd x))
                                                   && notTooHigh x current grid
                                                   && notAnyPrevious x previous] ++ previous
    where adj = [(spotEast current),(spotNorth current),(spotSouth current),(spotWest current)]
          notAnyPrevious x ys = if null [y | (y,z) <- ys, y == x && z >= 0] then True else False


coordToStepPair :: [[Int]] -> Coord -> [(Coord, Int)]    
coordToStepPair grid dest = [if (x,y) == dest then ((x,y),0) else ((x,y),(-1)) | y <- [0..((length grid)-1)], x <- [0..((length (grid!!y))-1)]]

--Instead of the starting coord, find any coord height 'a'
findXinGrid2 :: Int -> Int -> [[Int]] -> [(Coord,Int)] -> Int
findXinGrid2 startH x grid visited
     | containsVal visited startH grid = x
     | otherwise = findXinGrid2 startH (x+1) grid visited'
     where visited' = neighboursInReach x grid visited (coordsFilterX visited x)
           containsVal ys x grid = if not $ null [z | ((h,v),z) <- ys, z >= 0, ((grid!!v)!!h) == x ] then True else False


main = do
    inputTest <- readFile "topograph.test"
    input <- readFile "topograph.map"
    --Decide which input to use
    let grid = lines input
    --let grid = lines inputTest
    --find starting pos S
    let startPos = head $ findInGrid 'S' grid 
    let dest_e = head $ findInGrid 'E' grid
    --Convert the grid to height values in int
    let intGrid = map gridToInt grid
    let coordGrid = coordToStepPair intGrid dest_e
    --Find all paths
    let results = findXinGrid startPos 0 intGrid coordGrid
    let results2 = findXinGrid2 s_start 0 intGrid coordGrid
    --Remove any failed paths from results and find shortest
    print intGrid
    print results
    print results2
