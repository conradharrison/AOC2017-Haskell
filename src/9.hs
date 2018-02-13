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

data Token = SGRP | EGRP | SGARB | EGARB | IGNORENEXT | COMMA deriving (Eq, Enum, Show)
type ParserState = State (Stack Token, Int)

upLevel :: ParserState 
upLevel = do
            (s, l) <- get
            return (ns, l-1) where (Just ns) = pop s

downLevel :: ParserState 
downLevel = do
                (s, l) <- get
                return (push s SGRP, l+1)

keepLevel :: ParserState
keepLevel = do
                (s, l) <- get
                return (ns, l) where (Just ns) = pop s

pushToken :: Token -> ParserState
pushToken t = do
                (s, l) <- get
                return (push s t, l)

parse :: [Char] -> Int -> (ParserState, Int)
parse [] a       = (return (), a) 
parse (x:rest) a | (top == Just IGNORENEXT       ) = parse (let (_, ns) = pop s in ns) rest
                 | (top == Just SGARB && x == '>') = parse (push s EGARB) rest 
                 | (top == Just EGARB && x == ',') = parse (push s COMMA) rest 
                 | (top == Just EGARB && x == '}') = (downLevel, a)
                 | (top == Just SGRP  && x == '<') = parse (push s SGARB) rest 
                 | (top == Just SGRP  && x == '{') = (upLevel, a)
                 | (top == Just SGRP  && x == '}') = (downLevel, a)
                 | (top == Just EGRP  && x == '}') = (downLevel, a) 
                 | (top == Just EGRP  && x == ',') = (pushToken COMMA, a)
                 | (top == Just COMMA && x == '{') = (upLevel, a)
                 | (top == Just COMMA && x == '<') = (pushToken SGARB, a) 
                 | (top == Nothing    && x == '{') = (upLevel, a+1)
                 | (                     x == '!') = parse (push s IGNORENEXT) rest 
                 | otherwise                       = parse s rest 
                 where (top, _) = peek s

getScore :: Stack Token -> Int -> Int
getScore (Stack (x:xs)) currentLevel accum | x == SGRP = getScore (Stack (x:xs)) currentLevel+1 accum
                                           | x == EGRP = (Stack xs, (pop accum), top)
getScore (Stack []) c a = a

run :: [Char] -> Stack Token
run [] = Stack []
run l = parse (Stack []) l

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run inStr
