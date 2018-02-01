import System.Environment ( getArgs )
import Data.List

bool2int :: Bool -> Int
bool2int True = 1
bool2int False = 0

checkDupes :: String -> [String] -> Bool
checkDupes s ss = length (filter (==s) ss) == 0

checkAnagrams:: String -> [String] -> Bool
checkAnagrams s ss = length (filter (\x -> notAnagram s x) ss) == 0

notAnagram :: String -> String -> Bool
notAnagram a b = a `elem` (permutations b) 

processLine :: [String] -> Bool
processLine [s] = True
processLine (s:ss) = (checkDupes s ss) && processLine ss      -- > Part 1
--processLine (s:ss) = (checkAnagrams s ss) && processLine ss     -- > Part 2 

-- Count successes across rows
run :: [String] -> Int
run l = foldl (+) 0 lineValues
        where lineValues = bool2int <$> (processLine . words) <$> l

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run $ lines inStr
