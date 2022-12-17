import Data.List
import Data.Char
import Data.List.Split as Sp
import qualified Data.Set as Set

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

lowestObstacle :: [Coord] -> Int
lowestObstacle obstacles = maximum [snd y | y <- obstacles]

--Subtract one from empty grid pattern because result is reported by previous recursion that adds one after having found solution
newSand :: Int -> [Coord] -> Int -> Int
newSand x [] hAbyss = x - 1
newSand x grid hAbyss = newSand (x+1) grid' hAbyss where
    grid' = moveSand grid hAbyss sandSource
    sandSource = (500,0)

--Y coords are upside down, sand is "falling up." Use North to check below sand
moveSand :: [Coord] -> Int -> Coord -> [Coord]
moveSand grid hA current
    | (snd current) >= hA = []
    | noObs (spotNorth current) = moveSand grid hA (spotNorth current)
    | noObs $ (spotWest . spotNorth) current = moveSand grid hA ((spotWest . spotNorth) current)
    | noObs $ (spotEast . spotNorth) current = moveSand grid hA ((spotEast . spotNorth) current)
    | otherwise = current:grid
    where noObs x = not $ checkObstacle grid x

checkObstacle :: [Coord] -> Coord -> Bool
--checkObstacle grid coord = if null [x | x <- grid, x == coord] then False else True
checkObstacle grid coord = if coord `elem` grid then True else False

spotSouth :: Coord -> Coord
spotSouth (x,y) = (x,(y-1))

spotNorth :: Coord -> Coord
spotNorth (x,y) = (x,(y+1))

spotEast :: Coord -> Coord
spotEast (x,y) = ((x+1),y)

spotWest :: Coord -> Coord
spotWest (x,y) = ((x-1),y)


main = do
    input <- readFile "cave.in"
    inputTest <- readFile "cave.test"
    --let obstacles = concat $ map obstacleBuilder $ map parseIn $ lines inputTest
    let obstacles = concat $ map obstacleBuilder $ map parseIn $ lines input
    let result = newSand 0 obstacles (lowestObstacle obstacles)
    print result
