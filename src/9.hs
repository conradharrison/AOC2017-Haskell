import Prelude hiding (EQ, GT, LT)
import System.Environment ( getArgs )
import Debug.Trace

-- Helper class
data Stack a = Stack [a] deriving Show

push :: (Show a) => Stack a -> a -> Stack a
push (Stack s) c = Stack ((trace (show c) c):s)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

peek :: Stack a -> (Maybe a, Stack a)
peek (Stack []) = (Nothing, Stack [])
peek (Stack (x:xs)) = (Just x, Stack (x:xs))

-- End Helpers

data Thing s = Group s | Garbage s

data Token = SGRP | EGRP | SGARB | EGARB | IGNORENEXT | COMMA | OTHERS deriving (Eq, Enum, Show)

parse :: Stack Token -> [Char] -> Stack Token
parse s []       = s
parse s (x:rest) | (top == Just IGNORENEXT       ) = parse (let (_, ns) = pop s in ns) rest
                 | (top == Just SGARB && x == '>') = parse (push s EGARB) rest 
                 | (top == Just EGARB && x == ',') = parse (push s COMMA) rest 
                 | (top == Just EGARB && x == '}') = parse (push s EGRP) rest 
                 | (top == Just SGRP  && x == '<') = parse (push s SGARB) rest 
                 | (top == Just SGRP  && x == '{') = parse (push s SGRP) rest 
                 | (top == Just SGRP  && x == '}') = parse (push s EGRP) rest 
                 | (top == Just EGRP  && x == '}') = parse (push s EGRP) rest 
                 | (top == Just EGRP  && x == ',') = parse (push s COMMA) rest 
                 | (top == Just COMMA && x == '{') = parse (push s SGRP) rest 
                 | (top == Just COMMA && x == '<') = parse (push s SGARB) rest 
                 | (top == Nothing    && x == '{') = parse (push s SGRP) rest 
                 | (                     x == '!') = parse (push s IGNORENEXT) rest 
                 | otherwise                       = parse s rest 
                 where (top, _) = peek s

getScore :: Stack Token -> Stack Int -> (Stack Token, Stack Int, Int)
getScore (Stack (x:xs)) accum | x == SGRP = getScore xs (push accum (top+1))
                              | x == EGRP = (Stack xs, (pop accum), top)
getScore

run :: [Char] -> Stack Token
run [] = Stack []
run l = parse (Stack []) l

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run inStr
