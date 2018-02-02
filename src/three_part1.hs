import System.Environment ( getArgs )
import Debug.Trace

-- State of execution: (X, Y, index)
type PState = (Int, Int, Int)

-- Output at each state: if out of list bounds, then return count, else Nothing
type POutput = Maybe Int
outputFromState :: PState -> POutput
outputFromState (x, y, index) | index < target = Nothing
                              | otherwise = Just (abs x + abs y)

-- Update state - 
--      move to next location
--      increment value
nextState :: PState -> PState
nextState (x, y, index) = (newx, newy, newindex)
                          where (newx, newy) = getNextXY x y
                                newindex = index + 1

-- Do the spiral walk
getNextXY :: Int -> Int -> (Int, Int)
getNextXY 0 0 = (1, 0)
getNextXY x y |  x > y  && -y < x   = (x, y+1)
              | -x < y  &&  x <= y  = (x-1, y)
              | -x > -y && -x >= y  = (x, y-1)
              |  x < -y && -x <= -y = (x+1, y)
              | -y == x             = (x+1, y)
                                     
-- Start the state machine. Terminate when "Just" is returned. Keep running if "Nothing" is returned
run :: PState -> Int
run s = case outputFromState s of
            Nothing -> run $ nextState s
            Just c -> c

target = 325489

main :: IO()
main = do
        print $ let initState = (0, 0, 1) in
                run $ initState
