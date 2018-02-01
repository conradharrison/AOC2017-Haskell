import System.Environment ( getArgs )

maxDiffs :: [Int] -> Int
maxDiffs l = (maximum l - minimum l)

evenDivide :: [Int] -> Int
evenDivide l = filterAndDivide $ allPairs l

-- For each tuple in the list, check for divisibility and return quotient (first-found)
filterAndDivide :: [(Int, Int)] -> Int
filterAndDivide (x:xs) | (a `mod` b) == 0 = a `div` b
                       | otherwise = filterAndDivide xs
                       where (a, b) = x

-- Get all tuples within the list
allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs [x] = []
allPairs (x:xs) = (foldl (++) [] (map (\e -> [(e, x), (x, e)]) xs)) ++ allPairs xs

processRow = maxDiffs       -- > Part 1
--processRow = evenDivide     -- > Part 2

toInts :: String -> [Int]
toInts s = map (\x -> read x :: Int) $ words s

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ foldl (+) 0 $ map processRow $ map toInts (lines inStr)
