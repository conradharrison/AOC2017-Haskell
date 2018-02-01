import System.Environment ( getArgs )

-- State of execution: (list of past lists, current list, count)
type PState = ([[Int]], [Int], Int)

-- Output at each state: if out of list bounds, then return count, else Nothing
type POutput = Maybe Int
outputFromState :: PState -> POutput
outputFromState (pasts, current, count) | not (current `elem` pasts) = Nothing
                                        | otherwise = Just count

-- Update state (do one redistribution)
nextState :: PState -> PState
nextState (pasts, current, count) = (pasts ++ [current], newlist, newcount)
                                     where newlist = redistribute current
                                           newcount = count + 1
-- Main Algo to redistribute
redistribute :: [Int] -> [Int]
redistribute l = mapInd (\x i -> x + (getIncr n c startPoint i)) l
                 where startPoint = pickStartPoint l
                       n = length l
                       c = l !! (pickStartPoint l)

-- How much should each index increment by?
getIncr n c s i | i == s    = q + (isIncrIndex i) - c
                | otherwise = q + (isIncrIndex i)
                where q = c `div` n
                      r = c `mod` n
                      isIncrIndex i | wrapped i > wrapped s && wrapped i <= wrapped (s+r) = 1
                                    | otherwise = 0
                                    where wrapped x | s+r >= n && x < s = x+n
                                                    | otherwise = x
pickStartPoint :: [Int] -> Int
pickStartPoint l = fst $ firstMax l

firstMax :: (Ord a) => [a] -> (Int, a)
firstMax [] = error "Empty list to firstMax"
firstMax [x] = (0, x)
firstMax (x:xs) | x >= firstMaxValue = (0, x)
                | otherwise = (1 + firstMaxIndex, firstMaxValue)
                where (firstMaxIndex, firstMaxValue) = firstMax xs

-- Own version of map that passes "current index" into the map function
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]
                                     
-- Execute entire program. Terminate when "Just" is returned. Keep running if "Nothing" is returned
run :: PState -> Int
run s = case outputFromState s of
            Nothing -> run $ nextState s
            Just c -> c

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        instr <- readFile (head args)
        print $ let ilist = map (\s -> read s :: Int) (words instr)
                    initState = ([[]], ilist, 0) in
                run $ initState
