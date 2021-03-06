import System.Environment ( getArgs )

-- State of execution: (instruction list, instruction pointer, instruction count)
type PState = ([Int], Int, Int)

-- Output at each state: if out of list bounds, then return count, else Nothing
type POutput = Maybe Int
outputFromState :: PState -> POutput
outputFromState (ilist, ip, count) | ip >= 0 && ip < (length ilist) = Nothing
                                   | otherwise = Just count

-- Update state (a.k.a execute one instruction) using two different update rules
-- 1. "udpateInstr1"
-- 2. "updateInstr2"
nextState :: PState -> PState
nextState (ilist, ip, count) = (newlist, newip, newcount)
                               where newip = (ip+ilist!!ip)
                                     newlist = take ip ilist ++ [updateInstr2 (ilist!!ip)] ++ drop (ip+1) ilist
                                     newcount = count + 1

-- Update rules
-- option #1
updateInstr1 :: Int -> Int
updateInstr1 i = i + 1

-- option #2
updateInstr2 :: Int -> Int
updateInstr2 i | i < 3       = i + 1
               | otherwise   = i - 1
                                     
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
        print $ let ilist = map (\s -> read s :: Int) (lines instr)
                    initState = (ilist, 0, 0) in
                run $ initState
