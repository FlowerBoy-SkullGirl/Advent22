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
newSand :: Int -> Set.Set Coord -> Int -> Int
newSand x grid hAbyss 
    | grid == Set.empty = x - 1
    | otherwise = newSand (x+1) grid' hAbyss 
    where grid' = moveSand grid hAbyss sandSource
          sandSource = (500,0)

newSand2 :: Int -> Set.Set Coord -> Int -> Int
newSand2 x grid hAbyss 
    | grid == Set.empty = x - 1
    | otherwise = newSand2 (x+1) grid' hAbyss
    where grid' = moveSand2 grid hAbyss sandSource
          sandSource = (500,0)

--Y coords are upside down, sand is "falling up." Use North to check below sand
moveSand :: Set.Set Coord -> Int -> Coord -> Set.Set Coord
moveSand grid hA current
    | (snd current) >= hA = Set.empty
    | noObs (spotNorth current) = moveSand grid hA (spotNorth current)
    | noObs $ (spotWest . spotNorth) current = moveSand grid hA ((spotWest . spotNorth) current)
    | noObs $ (spotEast . spotNorth) current = moveSand grid hA ((spotEast . spotNorth) current)
    | otherwise = Set.insert current grid
    where noObs x = not $ checkObstacle grid x

moveSand2 :: Set.Set Coord -> Int -> Coord -> Set.Set Coord
moveSand2 grid hA current
    | checkObstacle grid spotSource = Set.empty
    | (snd $ spotNorth current) == hA = Set.insert current grid
    | noObs (spotNorth current) = moveSand2 grid hA (spotNorth current)
    | noObs $ (spotWest . spotNorth) current = moveSand2 grid hA ((spotWest . spotNorth) current)
    | noObs $ (spotEast . spotNorth) current = moveSand2 grid hA ((spotEast . spotNorth) current)
    | otherwise = Set.insert current grid
    where noObs x = not $ checkObstacle grid x
          spotSource = (500,0)

checkObstacle :: Set.Set Coord -> Coord -> Bool
--checkObstacle grid coord = if null [x | x <- grid, x == coord] then False else True
checkObstacle grid coord = if coord `Set.member` grid then True else False

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
    let obstacles = Set.fromList $ concat $ map obstacleBuilder $ map parseIn $ lines input
    let heightAbyss = lowestObstacle (Set.toList obstacles)
    let result = newSand 0 obstacles heightAbyss
    let result2 = newSand2 0 obstacles (heightAbyss + 2)
    print result
    print result2
