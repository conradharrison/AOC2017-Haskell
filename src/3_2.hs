import System.Environment ( getArgs )

-- State of execution: (X, Y, mem, value)
type PState = (Int, Int, [Int], Int)

-- Output at each state: if out of list bounds, then return value, else Nothing
type POutput = Maybe Int
outputFromState :: PState -> POutput
outputFromState (x, y, mem, value) | value < target = Nothing
                                   | otherwise = Just value
                                   where terminationCondition = value < target

-- Update state - 
--    move to the new location
--    compute next value (sum of neighbours of next location)
--    update the mem with the value
nextState :: PState -> PState
nextState (x, y, mem, value) = (newx, newy, newmem, newvalue)
                               where (newx, newy) = getNextXY x y
                                     newvalue = getNextValue newx newy mem
                                     newmem = mem ++ [newvalue]

-- Given a 2D index (x, y), return the mem value at that location; 0 if not yet written
getNextValue :: Int -> Int -> [Int] -> Int 
getNextValue x y mem = (linearIndexLookup mem (twoDIndex (x+1) (y  ))) +
                       (linearIndexLookup mem (twoDIndex (x+1) (y+1))) +
                       (linearIndexLookup mem (twoDIndex (x  ) (y+1))) +
                       (linearIndexLookup mem (twoDIndex (x-1) (y+1))) +
                       (linearIndexLookup mem (twoDIndex (x-1) (y  ))) +
                       (linearIndexLookup mem (twoDIndex (x-1) (y-1))) +
                       (linearIndexLookup mem (twoDIndex (x  ) (y-1))) +
                       (linearIndexLookup mem (twoDIndex (x+1) (y-1))) 

linearIndexLookup :: [Int] -> Int -> Int
linearIndexLookup mem i = if i < length mem then mem !! i else 0

-- Given a 2D index (x, y), return a (spiraling) linear index
twoDIndex :: Int -> Int -> Int
twoDIndex 0 0 = 0
twoDIndex x y |  x > y  && -y < x   = (sq (2*k-1))           + (k+y) - 1
              | -x < y  &&  x <= y  = (sq (2*k-1)) +   (2*k) + (k-x) - 1
              | -x > -y && -x >= y  = (sq (2*k-1)) + 2*(2*k) + (k-y) - 1
              |  x < -y && -x <= -y = (sq (2*k-1)) + 3*(2*k) + (k+x) - 1
              | -y == x             = (sq (2*k+1))                   - 1
              where sq r = r * r
                    k = max (abs x) (abs y) 

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
        print $ let initState = (0, 0, [1], 1) in
                run $ initState
