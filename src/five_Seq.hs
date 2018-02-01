import System.Environment ( getArgs )
import qualified Data.Sequence as Seq


-- Execute entire program, incrementing counter everytime
-- (instructions, pointer, count) -> final_count
exec :: (Seq.Seq Int, Int) -> Int -> Int
exec (ilist, ip) c  | ip >= 0 && ip < (Seq.length ilist) = exec (step (ilist, ip)) c+1
                    | otherwise = c

-- Execute one instruction, change state
-- (instructions, pointer) -> (instructions, pointer) 
step :: (Seq.Seq Int, Int) -> (Seq.Seq Int, Int)
step (ilist, ip) = (newlist, newip)
                   where newip = (ip + (Seq.index ilist ip))
                         newlist = Seq.update ip ((Seq.index ilist ip)+1) ilist 

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        instr <- readFile (head args)
        print $ let ilist = Seq.fromList $ map (\s -> read s :: Int) (lines instr) in
                exec (ilist, 0) 0

