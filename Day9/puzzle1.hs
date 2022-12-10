import Data.List
import Data.Char
import qualified Data.Set as Set

type Coord = (Int, Int)

parseStep :: String -> (Char, Int)
parseStep s = ((head(head s')),(read (last s') :: Int)) where
    s' = words s

distancesCoords :: Coord -> Coord -> (Int, Int)
distancesCoords x y = ((abs ((fst x)-(fst y))),(abs ((snd x)-(snd y))))

distanceO2 :: (Int, Int) -> Bool
distanceO2 (x,y) = if ((abs x) > 1) || ((abs y) > 1) then True else False

isNorthby2 :: Coord -> Coord -> Bool
isNorthby2 x y = if ((snd y) - (snd x)) == 2 then True else False

isSouthby2 :: Coord -> Coord -> Bool
isSouthby2 x y = if ((snd y) - (snd x)) == (-2) then True else False

isEastby2 :: Coord -> Coord -> Bool
isEastby2 x y = if ((fst y) - (fst x)) == 2 then True else False

isWestby2 :: Coord -> Coord -> Bool
isWestby2 x y = if ((fst y) - (fst x)) == (-2) then True else False

spotSouth :: Coord -> Coord
spotSouth (x,y) = (x,(y-1))

spotNorth :: Coord -> Coord
spotNorth (x,y) = (x,(y+1))

spotEast :: Coord -> Coord
spotEast (x,y) = ((x+1),y)

spotWest :: Coord -> Coord
spotWest (x,y) = ((x-1),y)

doSteps :: Int -> [(Char,Int)] -> Coord -> Coord -> [Coord] -> [Coord]
doSteps x steppy t h tracer 
    | x == (length steppy) = tracer
    | otherwise = doSteps (x+1) steppy t' h' tracer'
        where t' = tState mp
              h' = hState mp
              tracer' = tsState mp
              mp = movePoints (steppy!!x) t h tracer

doSteps10 :: Int -> [(Char,Int)] -> [Coord] -> [Coord] -> [Coord]
doSteps10 x steppy th tracer 
    | x == (length steppy) = tracer
    | otherwise = doSteps10 (x+1) steppy th' tracer'
        where th' = thState10 mp
              tracer' = tsState10 mp
              mp = movePoints10 (steppy!!x) th tracer

tState :: (Coord,Coord,[Coord]) -> Coord
tState (t,_,_) = t

hState :: (Coord,Coord,[Coord]) -> Coord
hState (_,h,_) = h

tsState :: (Coord,Coord,[Coord]) -> [Coord]
tsState (_,_,ts) = ts

thState10 :: ([Coord],[Coord]) -> [Coord]
thState10 (th,_) = th

tsState10 :: ([Coord],[Coord]) -> [Coord]
tsState10 (_,ts) = ts

moveH :: Char -> Coord -> Coord
moveH d h 
    | d == 'U' = (spotNorth h)
    | d == 'D' = (spotSouth h)
    | d == 'L' = (spotWest h)
    | d == 'R' = (spotEast h)

moveT :: Coord -> Coord -> Coord
moveT t h 
    | distanceO2 (distancesCoords t h) = findDirection t h
    | otherwise = t

moveT10 :: [Coord] -> [Coord]
moveT10 [] = []
moveT10 (h:[]) = []
moveT10 (h:t:[]) = (moveT t h):[]
moveT10 (h:t:ts) = (t'):(moveT10(t':ts)) where
    t' = moveT t h

movePoints :: (Char,Int) -> Coord -> Coord -> [Coord] -> (Coord,Coord,[Coord])
movePoints (d,n) t h tracer
    | n == 0 = (t,h,tracer) 
    | otherwise = movePoints (d,(n-1)) t' h' ((t'):tracer)
        where h' = moveH d h
              t' = moveT t h' 

movePoints10 :: (Char,Int) -> [Coord] -> [Coord] -> ([Coord],[Coord])
movePoints10 (d,n) th tracer
    | n == 0 = (th,tracer) 
    | otherwise = movePoints10 (d,(n-1)) th' ((last (th')):tracer)
        where th' = (moveH d (head th)):(moveT10 th)

findDirection :: Coord -> Coord -> Coord
findDirection t h
    | (isNorthby2 t h) && (isEastby2 t h) = spotSW h
    | (isNorthby2 t h) && (isWestby2 t h) = spotSE h
    | (isSouthby2 t h) && (isEastby2 t h) = spotNW h
    | (isSouthby2 t h) && (isWestby2 t h) = spotNE h
    | isNorthby2 t h = spotSouth h
    | isSouthby2 t h = spotNorth h
    | isEastby2 t h = spotWest h
    | isWestby2 t h = spotEast h
    | otherwise = t

spotSW :: Coord -> Coord
spotSW h = (spotSouth . spotWest) h

spotSE :: Coord -> Coord
spotSE h = (spotSouth . spotEast) h

spotNW :: Coord -> Coord
spotNW h = (spotNorth . spotWest) h

spotNE :: Coord -> Coord
spotNE h = (spotNorth . spotEast) h

main = do
    input <- readFile "rope.gps"
    --input <- readFile "rope.test"
    let steps = lines input
    let stepPairs = map parseStep steps
    let results = doSteps 0 stepPairs (0,0) (0,0) []
    let rset = Set.fromList results
    let res10 = doSteps10 0 stepPairs [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)] []
    let set10 = Set.fromList res10
    print stepPairs
    print results
    --print rset
    print (length rset)
    print (length stepPairs)
    print set10
    print (length set10)
