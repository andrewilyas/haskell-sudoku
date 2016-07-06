import Data.Char
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.Function
import Data.Maybe

--puzzle = "x8x5xxxxxxx6xxxx5x5x4x2xx91xx28xx9x6xxx7x2xxx8x1xx32xx92xx1x3x5x1xxxx6xxxxxxx5x7x"
puzzle = "13xxx6xxxx74xx258xxxxx5x3xxx8xx1xxxxxxxx6xx29xxxxxx43xxxxx3xx5x9x3xxx7x4xxxx758xx"
-- peer computation
row :: Int -> [Int]
row i = let r = (floor (fromIntegral i/9)) in [r*9..r*9+8]

col :: Int -> [Int]
col i = let c = i `mod` 9 in [c,c+9..c+72]

square :: Int -> [Int]
square i =
    let cfs = floor (fromIntegral (i `mod` 9)/3)
        rfs = floor (fromIntegral (floor (fromIntegral i/9))/3)
        cs = [cfs*3..cfs*3+2]
        rs = [rfs*3..rfs*3+2]
    in [c + r*9 | r <- rs, c <- cs]

peers i = square i `union` col i `union` row i

-- utils
delFrom :: Maybe [Int] -> Int -> [Int]
delFrom Nothing elem = []
delFrom list elem = [x | x <- (fromJust list), x /= elem]

-- constraint propagation
eliminate :: Map Int [Int] -> [Int] -> Int -> Map Int [Int]
eliminate puz peers val = Map.union (Map.fromList [(p, (delFrom (Map.lookup p puz) val)) | p <- peers]) puz

assign :: Map.Map Int [Int]-> Int -> Int -> Map.Map Int [Int]
assign puz ind val = Map.insert ind [val] (eliminate puz (peers ind) val)

state :: Map.Map Int [Int] -> Int
state puz
    | (f > 0) = 0
    | (o == (Map.size puz)) = 2
    | otherwise = 1
    where f = (length [1 | x <- [0..(Map.size puz)-1], (length (Map.findWithDefault [1..9] x puz)) == 0])
          o = (length [1 | x <- [0..(Map.size puz)-1], length (Map.findWithDefault [1..9] x puz) == 1])

minCell :: Map.Map Int [Int] -> Int
minCell puz = let w = [(Map.findWithDefault [1..9] i puz) | i <- [0..(Map.size puz)-1]] in findMin w (0, 1)

findMin :: [[Int]] -> (Int, Int) -> Int
findMin [x] (i, j) = i
findMin (x:y:xs) (i, j) 
    | length x == 1 && length y == 1 = findMin xs (j+1, j+2)
    | length y < length x = if length y > 1 then findMin (y:xs) (j, j+1) else findMin (x:xs) (i, j+1)
    | otherwise = if length x > 1 then findMin (x:xs) (i, j+1) else findMin (y:xs) (j, j+1)

search :: [Map.Map Int [Int]] -> Map.Map Int [Int]
search [] = Map.empty
search (puz:puzs)
    | s == 2 = puz
    | s == 1 = search ([assign puz (minCell puz) i | i <- (Map.findWithDefault [1..9] (minCell puz) puz)] ++ puzs)
    | otherwise = search puzs
    where
        s = state puz

emptyMap :: Map.Map Int [Int]
emptyMap = Map.fromList (zip [0..80] (take 81 (repeat [1..9])))

parse :: Map.Map Int [Int] -> String -> Int -> Map.Map Int [Int]
parse m "" i = m
parse m (p:ps) i
    | p /= 'x' = parse (assign m i (digitToInt p)) ps (i+1)
    | otherwise = parse m ps (i+1)

pMap :: Map.Map Int [Int] -> String -> Map.Map Int [Int]
pMap m p = parse m p 0

