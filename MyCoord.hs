module MyCoord where
type Coord = (Int, Int)

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

allBetween :: Coord -> Coord -> [Coord]
allBetween (x,y) (h,v) = [(x',y') | x' <- hor, y' <- ver] where
    hor = if x <= h then [x..h] else [h..x]
    ver = if y <= v then [y..v] else [v..y]


