import Data.Char
import Data.List
import Debug.Trace
import Data.Function

puzzle = "x8x5xxxxxxx6xxxx5x5x4x2xx91xx28xx9x6xxx7x2xxx8x1xx32xx92xx1x3x5x1xxxx6xxxxxxx5x7x"

-- peer computation
row :: Int -> [Int]
row i = let r = (floor (fromIntegral i/9)) in [r..r+8]

col :: Int -> [Int]
col i = let c = i `mod` 9 in [c,c+9..c+72]

square :: Int -> [Int]
square i =
    let cfs = floor (fromIntegral (i `mod` 9))
        rfs = floor (fromIntegral (floor (fromIntegral i/9)))
        cs = [cfs*3..cfs*3+2]
        rs = [rfs*3..rfs*3+2]
    in [c + r*9 | r <- rs, c <- cs]

peers i = square i `union` col i `union` row i

-- utils
delFrom :: [Int] -> Int -> [Int]
delFrom list elem = [x | x <- list, x /= elem]

-- constraint propagation
assign :: [[Int]] -> Int -> Int -> [[Int]]
assign puz ind val = [if i == ind then [val] else if i `elem` (peers ind) then (delFrom (puz!!i) val) else puz!!i | i <- [0..length puz - 1]]

state :: [[Int]] -> Int
state puz
    | (f > 0) = 0
    | (o == (length puz)) = 2
    | otherwise = 1
    where f = (length [1 | x <- puz, length x == 0])
          o = (length [1 | x <- puz, length x == 1])

findMin :: [[Int]] -> (Int, Int) -> Int
findMin [x, y] (i, j) = if length x < length y then i else j
findMin (x:y:xs) (i, j)
    | length x > length y = findMin (y:xs) (j, j+1)
    | otherwise = findMin (x:xs) (i, j+1)

-- search
search :: [[[Int]]] -> [[Int]]
search (puz:puzs)
    | s == 2 = puz
    | s == 1 = search ([assign puz (findMin puz (0,1)) i | i <- [1..9]] ++ puzs)
    | otherwise = search puzs
    where
        s = state puz

parse :: String -> [[Int]]
parse p = [if c == 'x' then [1..9] else [digitToInt c] | c <- p]

