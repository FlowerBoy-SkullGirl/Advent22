import Data.List
import Data.Char
import Data.List.Split as Sp

type Coord = (Int, Int)

parseIn :: String -> [Coord]
parseIn s = map stringToCoord $ Sp.splitOn " -> " s where
    stringToCoord st = ((read (head $ coordSplit st)::Int),(read (last $ coordSplit st)::Int))
    coordSplit st = Sp.splitOn "," st 

obstacleBuilder :: [Coord] -> [Coord]
obstacleBuilder [] = []
obstacleBuilder (o:[]) = []
obstacleBuilder (obst:next:[]) = allBetween obst next
obstacleBuilder obstacles = (allBetween (head obstacles) (head obstacles')) ++ (obstacleBuilder obstacles') where
    obstacles' = drop 1 obstacles

allBetween :: Coord -> Coord -> [Coord]
allBetween (x,y) (h,v) = [(x',y') | x' <- hor, y' <- ver] where
    hor = if x <= h then [x..h] else [h..x]
    ver = if y <= v then [y..v] else [v..y]

makeGrid :: Int -> [[Int]]
makeGrid x = replicate x [0 | y <- [0..x]]

addObstacle :: [[Int]] -> Coord -> [[Int]]
addObstacle grid coord = intsSplit (length grid) [if coord == (x,y) then 1 else (grid!!y)!!x | y <- [0..((length grid)-1)], x <- [0..((length (grid!!y))-1)]]

intsSplit :: Int -> [Int] -> [[Int]]
intsSplit _ [] = []
intsSplit n xs = (x:(intsSplit n xs')) where
    x = take n xs
    xs' = drop n xs

gridmapper :: [Coord] -> [[Int]] -> [[Int]]
gridmapper [] grid = grid
gridmapper obstacles grid = addObstacle grid' (head obstacles) where
    grid' = gridmapper (drop 1 obstacles) grid

lowestObstacle :: [Coord] -> Int
lowestObstacle obstacles = maximum [snd y | y <- obstacles]

newSand :: Int -> [[Int]] -> Int -> Int
newSand x [] hAbyss = x
newSand x grid hAbyss = newSand (x+1) grid' hAbyss where
    grid' = moveSand grid hAbyss sandSource
    sandSource = (500,0)

--Y coords are upside down, sand is "falling up." Use North to check below sand
moveSand :: [[Int]] -> Int -> Coord -> [[Int]]
moveSand grid hA current
    | (snd current) >= hA = []
    | noObs (spotNorth current) = moveSand grid hA (spotNorth current)
    | noObs $ (spotWest . spotNorth) current = moveSand grid hA ((spotWest . spotNorth) current)
    | noObs $ (spotEast . spotNorth) current = moveSand grid hA ((spotEast . spotNorth) current)
    | otherwise = addObstacle grid current
    where noObs x = not $ checkObstacle grid x

checkObstacle :: [[Int]] -> Coord -> Bool
checkObstacle grid (x,y) = if (grid!!y)!!x == 1 then True else False

spotSouth :: Coord -> Coord
spotSouth (x,y) = (x,(y-1))

spotNorth :: Coord -> Coord
spotNorth (x,y) = (x,(y+1))

spotEast :: Coord -> Coord
spotEast (x,y) = ((x+1),y)

spotWest :: Coord -> Coord
spotWest (x,y) = ((x-1),y)


main = do
    --input <- readFile "cave.in"
    inputTest <- readFile "cave.test"
    let obstacles = concat $ map obstacleBuilder $ map parseIn $ lines inputTest
    let grid = gridmapper obstacles $ makeGrid 1000 where
    let result = newSand 0 grid (lowestObstacle obstacles)
    print result
