import Data.List
import Data.List.Extra (anySame)
import Data.Char

findPattern :: Int -> String -> String -> (Int, String)
findPattern 0 _  _ = (0, "Pattern not found")
findPattern x (c:cs) ps = if anySame ps then findPattern (x-1) cs ps' 
                                 else (x, ps) where
                                                ps' = c : (take 3 ps)

findMessage :: Int -> String -> String -> (Int, String)
findMessage 0 _  _ = (0, "Pattern not found")
findMessage x (c:cs) ps = if anySame ps then findMessage (x-1) cs ps' 
                                 else (x, ps) where
                                                ps' = c : (take 13 ps)
main = do
    input <- readFile "datastream"
    let inputClean = take ((length input)-1) input
    let pattern = findPattern ((length inputClean)-1) inputClean (take 4 inputClean)
    let message = findMessage ((length inputClean)-1) inputClean (take 14 inputClean)
    print pattern
    print (((length inputClean)-1) - (fst pattern))
    print message
    print (((length inputClean)-1) - (fst message))
