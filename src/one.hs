import System.Environment ( getArgs )
import Debug.Trace

skipLength :: [Int] -> Int
skipLength l = 1                    -- > Part 1
--skipLength l = length l `div` 2     -- > Part 2

solve :: [Int] -> Int
solve l = foldl (+) 0 $ zipWith (\x y -> if x==y then x else 0) l ((drop (skipLength l) l) ++ (take (skipLength l) l))

toInts :: String -> [Int]
toInts s = map (\x -> read [x] :: Int) $ s

-- Takes one commandline argument: filename
main :: IO()
main = do 
        args <- getArgs
        inStr <- readFile (head args)
        -- (lines) is needed to drop the new-line char at the end of inStr
        print $ solve $ trace (show (toInts (head $ lines inStr))) $ toInts (head $ lines inStr)
