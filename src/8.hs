import Prelude hiding (EQ, GT, LT)
import System.Environment ( getArgs )
import Debug.Trace

data RState = RState {register::String, value::Int} deriving Show
instance Eq RState where
    s1 == s2 = (value s1) == (value s2)
instance Ord RState where
    s1 <= s2 = (value s1) <= (value s2)

data Cmp = EQ | NE | LE | GE | LT | GT deriving (Show, Eq)
data Op  = INC | DEC deriving (Show, Eq)
data Condition = Condition String Cmp Int deriving Show
data Instruction = Instruction String Op Int Condition deriving Show

checkCondition :: [RState] -> Condition -> Bool
checkCondition s (Condition creg cop cval) = executeCondition (readRegister s creg) cop cval
                                             where executeCondition a cmp b | cmp == EQ = a == b
                                                                            | cmp == NE = a /= b
                                                                            | cmp == LE = a <= b
                                                                            | cmp == GE = a >= b
                                                                            | cmp == LT = a < b
                                                                            | cmp == GT = a > b
                                                                            | otherwise  = error "WTF?!"

readRegister :: [RState] -> String -> Int
readRegister [] _ = 0
readRegister s r | r `elem` (map register s) = value $ head $ (filter (\x -> (register x) == r) s)
                 | otherwise                 = 0
                

step :: [RState] -> Instruction -> [RState]
step s (Instruction reg op opval cond) | checkCondition s cond = updateState s (RState reg execute) 
                                       | otherwise             = s
                                       where execute | op == INC = (readRegister s reg) + opval
                                                     | op == DEC = (readRegister s reg) - opval
                                                     | otherwise   = error "WTF2?!"

updateState :: [RState] -> RState -> [RState]
updateState slist s = (filter (\x -> register x /= register s) slist) ++ [s]

lineToInstruction :: String -> Instruction
lineToInstruction s = let [r, op, opval, _, creg, cop, cval] = words s in
                      Instruction r (strToOp op) (read opval::Int) (Condition creg (strToCmp cop) (read cval::Int))

strToOp :: String -> Op
strToOp s | s == "inc" = INC
          | s == "dec" = DEC
          | otherwise  = error "illegal operator"

strToCmp :: String -> Cmp
strToCmp s | s == "==" = EQ
           | s == "!=" = NE
           | s == "<=" = LE
           | s == ">=" = GE
           | s == "<"  = LT
           | s == ">"  = GT
           | otherwise = error "illegal condition operator"

-- output = culprit node, and the correction
run :: [RState] -> [Instruction] -> [RState]
run s [] = s
run s (x:xs) = run (step s x) xs

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ maximum $ run [] $ map lineToInstruction (lines inStr)
