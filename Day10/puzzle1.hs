import Data.List
import Data.Char

--Noop passes same value forward, addx passes same value then next value
runInst :: String -> [Int] -> [Int]
runInst s xs
    | (take 4 s) == "noop" = (head xs):xs
    | otherwise = nx:(head xs):xs
        where nx = (read (last $ words s) :: Int) + (head xs)

runInstrToX :: Int -> [String] -> Int -> [Int] -> [Int]
runInstrToX cyc instr x res
    | cyc == (length instr) = res
    | cyc >= x = res'
    | otherwise = runInstrToX (cyc+1) instr x res'
        where res' = runInst (instr!!cyc) res

crtRender :: [(Int,Int)] -> [Char]
crtRender xs = map isLit xs where
    isLit x = if (abs ((fst x) - (snd x))) > 1 then '.' else '#'

break40 :: [a] -> [[a]]
break40 [] = []
break40 x = (take 40 x):(break40 (drop 40 x))

main = do
    input <- readFile "cpu.ins"
    --input <- readFile "test.ins"
    let instset = lines input
    print instset
    let resultlist = runInstrToX 0 instset 240 [1]
    let final = sum $ [ ((reverse resultlist)!!n)*(n+1) | n <- [19,59..219]]
    let crtTim = map zip' ((break40 . reverse) resultlist) where
        zip' = zip [0..]
    print ([ ((reverse resultlist)!!n) | n <- [19,59..219]])
    print $ reverse resultlist
    print final
    mapM_ putStrLn $ map crtRender crtTim
