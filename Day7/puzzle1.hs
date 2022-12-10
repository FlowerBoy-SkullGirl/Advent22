import Data.List
import Data.Char

--Parse a log of commands traversing a filesystem
--Create a tree structure that mimics the fs
--Determine which dir recursively contain < 100000 bytes

--Tree structure
--Root is a name 's' and a pair of lists ([dir],[files])
--Files are a tuple such that ("name", size)
type File = (String, Int)
data TTree a = EmptyTree
             | Tnode String (a,[TTree a]) 
             deriving (Eq,Ord,Show,Read)

addBranch :: TTree a -> TTree a -> TTree a
addBranch (Tnode s (f,d)) nd = Tnode s (f,(nd:d))

addDataList :: TTree [a] -> a -> TTree [a]
addDataList (Tnode s (f,d)) nf = Tnode s ((nf:f),d)

updateData :: TTree a -> a -> TTree a
updateData (Tnode s (_,d)) nf = Tnode s (nf,d)

firstT :: TTree a -> a
firstT (Tnode _ (a,_)) = a

sndT :: TTree a -> [TTree a]
sndT (Tnode _ (_,d)) = d

branchName :: TTree a -> String
branchName (Tnode s _) = s

{-searchTree :: String -> TTree a -> Maybe TTree a
searchTree s root = if (branchName root) == s 
                      then root  
                      else if (null r') then (if null (sndT root) then Nothing else head (map sT (sndT root))) else (head r')
                           where r' = [dir | dir <- (sndT root), (branchName dir) == s]
                                 sT = searchTree s
-}
searchTree :: String -> TTree a -> Maybe (TTree a)
searchTree s root 
    | (null r') = (if null (sndT root) then Nothing else (findSomething (map sT (sndT root)))) 
    | not (null r') = Just (head r')
    | (branchName root) == s = Just root  
    | otherwise = Nothing
    where r' = [dir | dir <- (sndT root), (branchName dir) == s]
          sT = searchTree s

changeD :: TTree a -> [String] -> TTree a
changeD root (d:[]) = root
changeD root path = changeD (jMay EmptyTree (searchTree (last path') root)) path' where
    path' = init path

findSomething :: [Maybe a] -> Maybe a
findSomething [] = Nothing
findSomething ((Just x):xs) = Just x
findSomething ((Nothing):xs) = findSomething xs

updateBranch :: TTree a -> String -> TTree a -> TTree a
updateBranch (Tnode s (f,d)) bname branch = Tnode s (f,d')
    where d' = [if (branchName dir) /= bname then dir else branch | dir <- d]

-- Take a root, branch to be updated, and the path to the branch
updateTree :: TTree a -> TTree a -> [String] -> TTree a
updateTree root nx (c:[]) = nx
updateTree root nx path = updateBranch root (last path') (updateTree (jMay EmptyTree (searchTree (last path') root)) nx path')
    where path' = init path

jMay :: a -> Maybe a -> a
jMay _ (Just x) = x
jMay y _ = y

sumNode :: TTree [File] -> Int
sumNode t = sum (map snd (firstT t)) + (sum (map sumNode (sndT t)))

sumNode' :: TTree [File] -> [(String,Int)]
sumNode' root = ((branchName root),(sumNode root)):(concat (map sumNode' (sndT root)))

pop1d :: [a] -> [a]
pop1d [] = []
pop1d (c:[]) = []
pop1d (c:cs) = cs

--Take the root, a working node, a working directory list of String, and a list of command lines
parseCommands :: Int -> TTree [File] -> TTree [File] -> [String] -> [String] -> TTree [File]
parseCommands x root wdn cwd commands
    | x == (length commands) = root
    | (take 7 command) == "$ cd .." = parseCommands y root (changeD root cwd') cwd' commands
    | (take 4 command) == "$ cd" = parseCommands y root (changeD root cwd'') cwd'' commands
    | (take 4 command) == "$ ls" = parseCommands y root wdn cwd commands 
    | (take 3 command) == "dir" = parseCommands y (updT nn' cwd) nn' cwd commands
    | otherwise = parseCommands y (updT nn cwd) nn cwd commands
    where command = (commands!!x)
          y = x + 1
          updT = updateTree root 
          updB = updateBranch root
          addB = addBranch root
          addBcn = addBranch wdn 
          addF = addDataList root
          addFcn = addDataList wdn
          sz = read (head (words command)) :: Int
          fn = last (words command)
          nn = addFcn ((fn,sz)::File)
          nn' = (addBcn (Tnode (drop 4 command) ([],[])))
          cwd'' = ((drop 5 command):cwd)
          cwd' = pop1d cwd

sumFilesInput :: String -> Int
sumFilesInput s = sum n where
   n = [read x :: Int | x <- (words s), isDigit (head x)]

directoriesTotal :: TTree a -> [String]
directoriesTotal t = (branchName t):(concat (map directoriesTotal (sndT t)))

main = do
    input <- readFile "unixshell.log"
    --input <- readFile "test.log"
    let history = lines input
    --index, root, current node, cwd, commands
    let root = parseCommands 0 (Tnode "/" ([],[])) (Tnode "" ([],[])) [] history
    print (sum (map snd (filter (\h -> (snd h) < 100000) (sumNode' root))))
    --print (filter (\h -> (snd h) < 100000) (sumNode' root))
    print (sumFilesInput input)
    let totalSpaceUsed = (sumNode root)
    let neededSpace = totalSpaceUsed - (70000000-30000000)
    let directoriesWithSpace = (filter (\h -> (snd h) >= neededSpace) (sumNode' root))
    print totalSpaceUsed
    print neededSpace
    print (minimum (map snd directoriesWithSpace))
