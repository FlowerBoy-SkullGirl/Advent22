import Data.List
import Data.Char

height_stacks = 8
num_stacks = 9
depth_instructions = height_stacks + 2
columns = [2,6..34]
wordsCommands = [1,3,5]

trimLine :: String -> String
trimLine s = [s!!(x-1) | x <- columns]

linesToStacks :: Int -> [String] -> String -> [String]
linesToStacks (-1) xs _ = xs
linesToStacks x xs s = linesToStacks (x-1) xs' s where
    xs' = if (s!!x) /= ' ' then pushLine xs else xs where
        pushLine xs = [if n /= x then xs!!n else (s!!x : xs!!x) | n <- [0..(num_stacks-1)]]

moveCommand :: (Int, Int, Int) -> [String] -> [String]
moveCommand (0,_,_) xs = xs
moveCommand (x,y,z) xs = moveCommand ((x-1),y,z) xs' where
    xs' = [if (n /= (y-1)) && (n /= (z-1)) then xs!!n else 
           (if n == (y-1) then popy else pushz) | n <- [0..(num_stacks-1)]] where
               popy = snd (popStack (xs!!(y-1)))
               pushz = (head (xs!!(y-1))) : (xs!!(z-1))

moveCommand2 :: (Int, Int, Int) -> [String] -> [String]
moveCommand2 (0,_,_) xs = xs
moveCommand2 (x,y,z) xs = [if (n /= (y-1)) && (n /= (z-1)) then xs!!n else 
                          (if n == (y-1) then popy else pushz)
                          | n <- [0..(num_stacks-1)]] where
                              popy = drop x (xs!!(y-1))
                              pushz = (take x (xs!!(y-1))) ++ (xs!!(z-1))
    
commandsTriple :: String -> (Int, Int, Int)
commandsTriple s = (x, y, z) where
    x = read (wordList!!(wordsCommands!!0)) :: Int where
        wordList = words s
    y = read (wordList!!(wordsCommands!!1)) :: Int where
        wordList = words s
    z = read (wordList!!(wordsCommands!!2)) :: Int where
        wordList = words s

callCommands :: Int -> [(Int,Int,Int)] -> [String] -> [String]
callCommands (-1) _ xs = xs
callCommands x is xs = callCommands (x-1) is xs' where
    xs' = moveCommand (is!!(ys!!x)) xs where
        ys = reverse [0..((length is)-1)]

callCommands2 :: Int -> [(Int,Int,Int)] -> [String] -> [String]
callCommands2 (-1) _ xs = xs
callCommands2 x is xs = callCommands2 (x-1) is xs' where
    xs' = moveCommand2 (is!!(ys!!x)) xs where
        ys = reverse [0..((length is)-1)]

popStack :: String -> (Char, String)
popStack [] = (' ',[])
popStack (s:xs) = (s,xs)

callLTS :: Int -> [String] -> [String] -> [String]
callLTS (-1) xs _ = xs
callLTS x xs sV = callLTS (x-1) xs' sV where
    xs' = linesToStacks (num_stacks-1) xs (sV!!x)

main = do
    input <- readFile "stacks.in"
    let diagram = lines input
    let stacksD = take height_stacks diagram
    let instructionsS = drop depth_instructions diagram
    let stacksV = map trimLine stacksD
    let stacks = callLTS ((length stacksV)-1) ["","","","","","","","",""] stacksV
    let instructions = map commandsTriple instructionsS
    let result = callCommands ((length instructions)-1) instructions stacks
    let result2 = callCommands2 ((length instructions)-1) instructions stacks
    print (map head result)
    print (map head result2)
