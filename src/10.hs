import System.Environment (getArgs)
import Data.Char (ord)
import Data.Bits
import Text.Printf
import Debug.Trace

-- splitOn delimiterString, inString -> [String]
splitOn :: String -> String -> [String]
splitOn d s = words (map (\c -> if c `elem` d then ' ' else c) s)

-- get slice of a list
fromTo :: Int -> Int -> [a] -> [a]
fromTo from to l = drop from (take (to+1) l)

-- length -> current position -> old chain -> new chain
runAlgo :: (Show a) => Int -> Int -> [a] -> [a]
runAlgo x c l | x==0 || x==1 = l
              | c+x >= chainLength = runAlgo_wrap x c l
              | otherwise          = runAlgo_nowrap x c l

-- length -> current position -> old chain -> new chain
runAlgo_wrap :: (Show a) => Int -> Int -> [a] -> [a]
runAlgo_wrap x c l = (drop (chainLength - c) rl) ++ (fromTo ((c+x)`mod`chainLength) (c-1) l) ++ (take (chainLength - c) rl)
                     where rl = reverse $ fromTo c (c+x-1) (l ++ l)

-- length -> current position -> old chain -> new chain
runAlgo_nowrap :: (Show a) => Int -> Int -> [a] -> [a]
runAlgo_nowrap x c l = (take c l) ++ rl ++ (drop (c+x) l)
                       where rl = reverse $ fromTo c (c+x-1) l

chain = [0..255]
chainLength = 256

-- list of lenghts -> current position -> skip size -> old chain -> new chain
twist :: (Show a) => [Int] -> Int -> Int -> [a] -> [a]
twist []     _ _ old = old
twist (x:xs) c s old = twist xs nextc nexts new
                       where nexts = s+1
                             nextc = (c + x + s) `mod` (length old)
                             new = runAlgo x c old

sparseHash :: [Int] -> [Int]
sparseHash [] = []
sparseHash l = [foldl (xor) 0 (take 16 l)] ++ (sparseHash (drop 16 l))

showAsHex :: [Int] -> String
showAsHex l = foldl (\x y -> (printf "%s%02x" x y)) "" l

run :: [Int] -> String
run l = showAsHex $ sparseHash $ twist lengths 0 0 chain
                                 where lengths = foldl (++) [] (replicate 64 l)
--Part 1
--run :: [Int] -> Int
--run l = foldl (*) 1 $ take 2 (twist l 0 0 chain)

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        --print $ run1 $ map (\x -> (read x :: Int)) (splitOn ", " inStr) -- Part 1
        print $ run $ (map ord (head $ lines $ inStr)) ++ [17,31,73,47,23]

