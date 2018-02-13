import System.Environment ( getArgs )
import Control.Monad.State
import qualified Data.Map.Strict as M
import Prelude hiding (EQ, GT, LT)

data Cmp = EQ | NE | LE | GE | LT | GT deriving (Show, Eq)
data Op  = INC | DEC deriving (Show, Eq)
data Condition = Condition String Cmp Int deriving Show
data Instruction = Instruction String Op Int Condition deriving Show

-- Store all our variables, and a max register
type SymbolTableAndMax = (M.Map String Int, Int)

-- Executor State monad
type Executor a = State SymbolTableAndMax a

-- The Executor monad functions are:
--  lookup
--  addSymbol
--  execute
--  checkCondition
--  parse

lookUp :: String -> Executor Int
lookUp str = do
                (symbolTable, _) <- get
                case M.lookup str symbolTable of
                    Just v -> return v
                    Nothing -> return 0

addSymbol :: String -> Int -> Executor () 
addSymbol str val = do
                        (symbolTable, m) <- get
                        put $ (M.insert str val symbolTable, if val>m then val else m)

execute :: Instruction -> Executor ()
execute (Instruction reg op opval cond) = do
                                            r <- lookUp reg
                                            cond <- checkCondition cond
                                            case cond of
                                                True -> case op of
                                                            INC -> addSymbol reg (r + opval)
                                                            DEC -> addSymbol reg (r - opval)
                                                False -> return ()
                    
checkCondition :: Condition -> Executor Bool
checkCondition (Condition creg cop cval) = do
                                            r <- lookUp creg
                                            case cop of
                                                EQ -> return (r == cval)                                            
                                                NE -> return (r /= cval)                                            
                                                LE -> return (r <= cval)                                            
                                                GE -> return (r >= cval)                                            
                                                LT -> return (r  < cval)                                            
                                                GT -> return (r  > cval)                                            

-- Parse file text to an Instruction
parse :: String -> Executor Instruction
parse s = let [r, op, opval, _, creg, cop, cval] = words s in
          return $ Instruction r (strToOp op) (read opval::Int) (Condition creg (strToCmp cop) (read cval::Int))

-- parse helper
strToOp :: String -> Op
strToOp s | s == "inc" = INC
          | s == "dec" = DEC
          | otherwise  = error $ "Illegal operator: " ++ s

-- parse helper
strToCmp :: String -> Cmp
strToCmp s | s == "==" = EQ
           | s == "!=" = NE
           | s == "<=" = LE
           | s == ">=" = GE
           | s == "<"  = LT
           | s == ">"  = GT
           | otherwise = error $ "Illegal condition operator: " ++ s

-- output = culprit node, and the correction
run :: [String] -> Executor ()
run [] = return ()
run (x:xs) = do
                instruction <- parse x
                execute instruction
                run xs 

-- Generic Map helper
-- Get the map key that has the largest value
keyOfMaxValue :: (Ord a, Ord b) => M.Map a b -> a
keyOfMaxValue m = findm (M.keys m) 
                  where findm []     = error "Cannot find max in empty map"
                        findm [x]    = x
                        findm (x:xs) | (m M.! x) > (m M.! (findm xs)) = x
                                     | otherwise                      = findm xs

-- Takes one commandline argument: filename
main :: IO ()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        let (_, (st, m)) = runState (run (lines inStr)) (M.fromList [], 0)
            kmax         = keyOfMaxValue st in
            print $ (show $ (kmax, (st M.! kmax))) ++ ", " ++ (show m)
