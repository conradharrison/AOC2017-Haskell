import System.Environment (getArgs)
import Debug.Trace

-- splitOn delimiterString, inString -> [String]
splitOn :: String -> String -> [String]
splitOn d s = words (map (\c -> if c `elem` d then ' ' else c) s)

data Dir = N | S | NE | NW | SE | SW deriving Show
data HexCoord = HexCoord (Int, Int) deriving Show

strToDir :: String -> Dir
strToDir s = case s of 
                "n"  -> N
                "s"  -> S
                "ne" -> NE
                "nw" -> NW
                "se" -> SE
                "sw" -> SW

-- track the walk (centeres of the hexagons) in regular cartesian coordinates.
walk :: [Dir] -> (HexCoord, HexCoord) -> (HexCoord, HexCoord)
walk [] o          = o
walk (x:xs) (h, hmax) = walk xs (nexth, nexthmax) 
                        where nexthmax = if ((hexDistance nexth) > (hexDistance hmax)) then nexth else hmax
                              nexth = case x of
                                         N  -> moveN h
                                         S  -> moveS h
                                         NE -> moveNE h
                                         NW -> moveNW h
                                         SE -> moveSE h
                                         SW -> moveSW h

moveN  (HexCoord (u, v)) = HexCoord (u  , v+2)
moveS  (HexCoord (u, v)) = HexCoord (u  , v-2)
moveNE (HexCoord (u, v)) = HexCoord (u+1, v+1)
moveNW (HexCoord (u, v)) = HexCoord (u-1, v+1)
moveSE (HexCoord (u, v)) = HexCoord (u+1, v-1)
moveSW (HexCoord (u, v)) = HexCoord (u-1, v-1)

hexDistance :: HexCoord -> Int
hexDistance (HexCoord (u, v)) = ((abs (u)) + (abs (v))) `div` 2

run :: [Dir] -> Int
--run l = hexDistance $ final where (final, max) = walk l ((HexCoord (0,0)), (HexCoord (0,0))) -- Part 1
run l = hexDistance $ max   where (final, max) = walk l ((HexCoord (0,0)), (HexCoord (0,0)))

-- Takes one commandline argument: filename
main :: IO()
main = do
        args <- getArgs
        inStr <- readFile (head args)
        --print $ run1 $ map (\x -> (read x :: Int)) (splitOn ", " inStr) -- Part 1
        print $ run $ (map strToDir (splitOn "," (head $ lines $ inStr)))

