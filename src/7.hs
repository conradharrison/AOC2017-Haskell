import System.Environment ( getArgs )
import Debug.Trace

data Node = Node {name::String, weight::Int, stack::[Node]} | Empty deriving (Show)
instance Eq Node where
    s1 == s2 = (treeSum s1) == (treeSum s2)
instance Ord Node where
    s1 <= s2 = (treeSum s1) <= (treeSum s2)

-- myWords delimiterString, inString -> [String]
splitOn :: String -> String -> [String]
splitOn d s = words (map (\c -> if c `elem` d then ' ' else c) s)

-- "gbyvdfh (155) -> xqmnq, iyoqt, dimle" -> "Node gbyvdfh 155 [xqmnq,iyoqt,dimle]"
convertLineToNode :: String -> Node
convertLineToNode str = Node n (read w :: Int) (map (\x -> Node x 0 []) s)
                        where (n:w:s) = splitOn ")(->," str

getParent :: [Node] -> Node -> Node
getParent [] n = Empty
getParent (x:xs) n | (name n) `elem` (map name (stack x)) = x
                   | otherwise = getParent xs n

getRoot :: [Node] -> Node
getRoot [] = Empty
getRoot (n:ns) | p == Empty = n
               | otherwise = getRoot ([p] ++ (filter (/= p) (n:ns)))
               where p = getParent ns n

-- Build a proper tree from the input node list
buildTree :: [Node] -> Node -> Node
buildTree ns (Node n w s) = Node n w (map (\x -> buildTree ns (getNode ns x)) s)

-- Retrieve weight, stack for any node, to build a proper tree
getNode :: [Node] -> Node -> Node
getNode [] _ = error "Cannot find a node in input list"
getNode ((Node n w s):xs) (Node rn rw rs) | rn == n = Node n w s
                                          | otherwise = getNode xs (Node rn rw rs)

-- Return weight of the node + node's stack
treeSum :: Node -> Int
treeSum Empty = 0        
treeSum (Node n w []) = w
treeSum (Node n w s) = foldl (+) w (map treeSum s)

-- A unbalanced stack may be due to am Excess, Deficit or we may not know yet
data Parity = Excess | Deficit | Dunno deriving (Eq)

-- Return culprit node's weight with delta
traceOddNodeOut :: Parity -> Node -> (Int, Int)
traceOddNodeOut _ (Node n w []) = (w, 0)
traceOddNodeOut p n | isAnyChildCulprit n = (weight o, getDelta op (map treeSum (stack n)))
                    | otherwise  = traceOddNodeOut op o
                    where (o, op) = outlier (stack n) p
                          getDelta par list | par == Dunno = error "Tree has to be imbalanced"
                                            | otherwise    = minimum list - maximum list

-- Generic helper to pick the unique outlier in a list
-- Takes a Ord list, with a hint (Excess, Deficit, Dunno)
outlier :: (Ord a) => [a] -> Parity -> (a, Parity)
outlier [x] _ =  (x, Excess) -- Anything would do
outlier l        (Excess)  = (maximum l, Excess)
outlier l        (Deficit) = (minimum l, Excess)
outlier [x, y]   (Dunno)   = (x, Excess) -- Anything will do
outlier (x:y:xs) (Dunno)   | x `elem` (y:xs) = outlier ((y:xs)++[x]) Dunno
                           | otherwise = (x, if (x>y) then Excess else Deficit)

-- Some child is a culprit if none of the grandchildren are culprits
isAnyChildCulprit :: Node -> Bool
isAnyChildCulprit n = foldl (&&) True (map (\x -> isBalanced (stack x)) (stack n)) 

-- There is no outlier in the stack (i.e., balanced)
isBalanced :: [Node] -> Bool
isBalanced [] = True
isBalanced ns = let l = map treeSum ns in 
                (minimum l == maximum l)

-- output = culprit node, and the correction
run :: [Node] -> Int
run nodes = x + y
            where (x, y) = traceOddNodeOut Dunno $ buildTree nodes $ getRoot nodes

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ let nodes = map (\s -> convertLineToNode s) (lines inStr) in
                run nodes
