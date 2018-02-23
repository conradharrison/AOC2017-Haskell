import System.Environment (getArgs)
import Debug.Trace

-- splitOn delimiterString, inString -> [String]
splitOn :: String -> String -> [String]
splitOn d s = words (map (\c -> if c `elem` d then ' ' else c) s)

-- get slice of a list
fromTo :: Int -> Int -> [a] -> [a]
fromTo from to l = drop from (take (to+1) l)

-- current position -> length -> old chain -> new chain
runAlgo :: (Show a) => Int -> Int -> [a] -> [a]
runAlgo x c l | x==0 || x==1 = l
              | e==c         = let rs = reverse $ (fromTo c (chainLength-1) l) ++ (fromTo 0 (e-1) l) in
                               (drop (chainLength-c) rs) ++ (take (chainLength-c) rs)
              | e > c        = take c l ++ (reverse $ fromTo c e l) ++ drop (e+1) l
              | e < c        = reverse1 ++ (fromTo (e+1) (c-1) l) ++ reverse2
              where e             = if ((c+x)>chainLength) then (c+x-chainLength) else (c+x-1)
                    reverse1      = drop (chainLength - c + 1) reverseString
                    reverse2      = take (e+1) reverseString
                    reverseString = reverse $ (fromTo c (chainLength-1) l) ++ (fromTo 0 e l)

-- list of lenghts -> current position -> skip size -> old chain -> new chain
twist :: (Show a) => [Int] -> Int -> Int -> [a] -> [a]
twist []     _ _ old = old
twist (x:xs) c s old = twist xs nextc nexts (trace (show (x, c, s, old, new)) new)
                       where nexts = s+1
                             nextc = (c + x + s) `mod` chainLength
                             new = runAlgo x c old

-- constant inputs
chain = [0..255]
chainLength = length chain

run :: [Int] -> Int
run l = foldl (*) 1 $ take 2 (twist l 0 0 chain)

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run $ map (\x -> (read x :: Int)) (splitOn ", " inStr)
