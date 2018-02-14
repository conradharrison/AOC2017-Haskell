import Prelude hiding (EQ, GT, LT)
import System.Environment (getArgs)

-- Helper class
data Stack a = Stack [a] deriving Show

push :: (Show a) => Stack a -> a -> Stack a
push (Stack s) c = Stack (c:s)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

peek :: Stack a -> (Maybe a, Stack a)
peek (Stack []) = (Nothing, Stack [])
peek (Stack (x:xs)) = (Just x, Stack (x:xs))
-- End Helpers

data Token = SGRP | EGRP | SGARB | EGARB | IGNORENEXT | COMMA deriving (Eq, Enum, Show)

type CurrentLevel = Int
type GroupScore = Int
type GarbageCount = Int

-- Use a stack to parse the stream. Need current running level to accumulate
parse :: Stack Token -> [Char] -> CurrentLevel -> GroupScore -> GarbageCount -> (GroupScore, GarbageCount)
parse s []       l a g = (a, g)
parse s (x:rest) l a g | (top == Just IGNORENEXT       ) = parse (let (_, ns) = pop s in ns) rest  l     a    g
                       | (                     x == '!') = parse (push s IGNORENEXT)         rest  l     a    g
                       | (top == Just SGARB && x == '>') = parse (push s EGARB)              rest  l     a    g
                       | (top == Just SGARB            ) = parse       s                     rest  l     a   (g+1)
                       | (top == Just EGARB && x == '{') = parse (push s SGRP)               rest (l+1)  a    g
                       | (top == Just EGARB && x == '}') = parse (push s EGRP)               rest (l-1) (a+l) g
                       | (top == Just EGARB && x == '<') = parse (push s SGARB)              rest  l     a    g
                       | (top == Just SGRP  && x == '{') = parse (push s SGRP)               rest (l+1)  a    g
                       | (top == Just SGRP  && x == '<') = parse (push s SGARB)              rest  l     a    g
                       | (top == Just SGRP  && x == '}') = parse (push s EGRP)               rest (l-1) (a+l) g
                       | (top == Just EGRP  && x == '{') = parse (push s EGRP)               rest (l+1)  a    g
                       | (top == Just EGRP  && x == '}') = parse (push s EGRP)               rest (l-1) (a+l) g
                       | (top == Just EGRP  && x == '<') = parse (push s SGARB)              rest  l     a    g
                       | (top == Just EGRP  && x == '>') = parse (push s EGRP)               rest  l     a    g
                       | (top == Nothing    && x == '{') = parse (push s SGRP)               rest (l+1)  a    g
                       | otherwise                       = parse       s                     rest  l     a    g
                       where (top, _) = peek s

run :: String -> (GroupScore, GarbageCount)
run str = parse (Stack []) str 0 0 0

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ run inStr
