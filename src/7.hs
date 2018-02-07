import System.Environment ( getArgs )
import Debug.Trace

data Node = Node {name::String, weight::Int, stack::[Node]} | Empty deriving (Show, Eq)

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
treeSum (Node n w []) = w
treeSum (Node n w s) = foldl (+) w (map treeSum s)

-- Return culprit node with delta
traceOddNodeOut :: Node -> (Node, Int)
traceOddNodeOut (Node n w []) = Node n w []
traceOddNodeOut n | isBalanced (stack n) = error "We shouldnt hit this"
                  | isBalanced (stack o) = (o, getDelta (map treeSum (stack n)))
                  | otherwise  = traceOddNodeOut o
                  where o = (min or max based on sign of getDelta) $ stack n

isBalanced :: [Node] -> Bool
isBalanced ns = let l = map treeSum ns in 
                (minumum l == maximum l)

average :: [Int] -> Int
average xs = (sum xs) `div` (length xs)

getDelta :: [Int] -> Int
getDelta l = (maximum l) - (minimum l)

delta :: [Int] -> Int
delta [] = error "Cannot do delta on empty set"
delta [x] = 0
delta [x, y] = x-y
delta (x:y:z:rest) | x<y && x<z =    x-y
                   | x>y && x>z =  -(x-y)
                   | y<x && y<z =    y-x
                   | y>x && y>z =  -(y-x)
                   | z<x && z<y =    z-x 
                   | z>x && z>y =  -(z-x)
                   | otherwise  = delta (y:z:rest)

-- output = list of culplrit nodes: list of (node-weight, subTree-weight)
run :: [Node] -> [(Int, Int)]
run nodes = let a = map weight $ unbalancedStack
                b = map treeSum $ unbalancedStack
                unbalancedStack = stack $ traceOddNodeOut $ buildTree nodes $ getRoot nodes in
            zip a b

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        print $ let nodes = map (\s -> convertLineToNode s) (lines inStr) in
                run nodes
