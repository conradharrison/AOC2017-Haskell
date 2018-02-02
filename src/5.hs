import System.Environment ( getArgs )

-- Execute one instruction, change state
-- (instructions, pointer) -> (instructions, pointer) 
step :: ([Int], Int) -> ([Int], Int)
step (ilist, ip) = (newlist, newip)
                   where newip = (ip+ilist!!ip)
                         newlist = take ip ilist ++ [(ilist!!ip)+1] ++ drop (ip+1) ilist

-- Execute entire program, incrementing counter everytime
-- (instructions, pointer, count) -> final_count
exec :: ([Int], Int) -> Int -> Int
exec (ilist, ip) c  | ip >= 0 && ip < (length ilist) = exec (step (ilist, ip)) c+1
                    | otherwise = c

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        instr <- readFile (head args)
        print $ let ilist = map (\s -> read s :: Int) (lines instr) in
                exec (ilist, 0) 0

