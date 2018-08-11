module TicTacToe (initGrid, iterReplGrid, parseStringGrid, parseNumGrid, sliceGrid, replaceGrid, getRow, getColumns, getDiagonalOne, getDiagonalTwo, getDiagonals) where
import Data.List(transpose, foldr)

initGrid :: [Integer]
-- May use repeat function from Haskell later
initGrid = [0,0,0,0,0,0,0,0,0]

iterReplGrid :: [String] -> [String] 
-- Note : X replaces 1, O replaces 25 as a hardcoded version of a String replace list comprehension
iterReplGrid grid = [if i == "1" then "[X]" else if i == "25" then "[O]" else "[_]" | i <- grid]

parseStringGrid :: [Integer] -> [String]
-- converts Integer Linear Matrix for the hardcode string replace
parseStringGrid grid = [show x | x <- grid]

parseNumGrid :: [String] -> [Integer]
-- reverts String Linear Matrix if necessary to Integer Linear Matrix
parseNumGrid grid = [read x :: Integer | x <- grid]

sliceGrid :: [a] -> [[a]]
-- Yes, this is kind of a hack... Could of had function that increase take and drop by 3 each time
sliceGrid grid = [take 3 grid, drop 3 $ take 6 grid, drop 6 $ take 9 grid]

reduceGrid :: [[a]] -> [a]
-- Post this solution to Stack Overflow?
reduceGrid grid = foldr (++) [] grid

replaceGrid :: [a] -> Int -> a -> [a]
-- An alternative to the default immutable data structures of Haskell
replaceGrid grid index player = (take (index - 1) grid ++ [player] ++ drop index grid)
    
getRow :: [[Integer]] -> Bool
--sum, similar to a foldr (+) 0, then find if the sum pattern is consisted of all Xs or Os
getRow grid = elem 3 [sum x | x <- grid] || elem 75 [sum x | x <- grid]

getColumns :: [[Integer]] -> Bool
getColumns grid = elem 3 [sum x | x <- transpose grid] || elem 75 [sum x | x <- grid]

getDiagonalOne :: [[Integer]] -> [Integer]
getDiagonalOne grid = [(grid !! 0 !! 0), (grid !! 1 !! 1), (grid !! 2 !! 2)]

getDiagonalTwo :: [[Integer]] -> [Integer]
getDiagonalTwo grid = [(grid !! 0 !! 2), (grid !! 1 !! 1), (grid !! 2 !! 0)]

getDiagonals :: [[Integer]] -> Bool
getDiagonals grid = elem 3 [sum x | x <- ([getDiagonalOne grid] ++ [getDiagonalTwo grid])]

