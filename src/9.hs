import Prelude hiding (EQ, GT, LT)
import System.Environment ( getArgs )
import Debug.Trace

-- Helper class
data Stack a = Stack [a] deriving Show

push :: Stack a -> a -> Stack a
push (Stack s) c = Stack (c:s)

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
parse s (x:rest) | (top == IGNORENEXT       ) = parse (let (_, ns) = pop s in ns) rest
                 | (top == SGARB && x == '>') = parse (push s EGARB) rest 
                 | (top == EGARB && x == ',') = parse (push s COMMA) rest 
                 | (top == EGARB && x == '}') = parse (push s EGRP) rest 
                 | (top == SGRP  && x == '<') = parse (push s SGARB) rest 
                 | (top == SGRP  && x == '{') = parse (push s SGRP) rest 
                 | (top == SGRP  && x == '}') = parse (push s EGRP) rest 
                 | (top == EGRP  && x == '}') = parse (push s EGRP) rest 
                 | (top == EGRP  && x == ',') = parse (push s COMMA) rest 
                 | (                x == '!') = parse (push s IGNORENEXT) rest 
                 | otherwise                  = parse s rest 
                 where top = case (peek s) of
                                (Just t, _)  -> trace (show t) t
                                (Nothing, _) -> OTHERS

run :: [Char] -> Stack Token
run [] = Stack []
run l = parse (Stack []) l

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run inStr
