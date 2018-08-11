module Main where
import System.Environment
import System.IO
import Data.List(transpose, intersperse)
import Control.Monad
import qualified Data.Map as Map
import TicTacToe (initGrid, iterReplGrid, parseStringGrid, parseNumGrid, sliceGrid, replaceGrid, getRow, getColumns, getDiagonalOne, getDiagonalTwo, getDiagonals)


-- NAA. Haskell is unforgiving.
-- NLA. Haskell is irrational.
-- NLA.  I made my own flatten function that support default ++ operater for generic types
-- NLA.  I found an interesting hack for "a" and getting the compiloer to relax

--assureInput :: [Integer] -> IO String -- prevents input from overlapping on grid
--assureInput grid = do
  --input <- (assureInput grid)
  --let index = (read input :: Int)
  --if (grid !! (index-1) /= 0) then return input else assureInput grid

playGrid :: [Integer] -> IO a
playGrid grid = do  
  --input <- (assureInput grid)
  input <- getLine
  let index = (read input :: Int)
  mapM_ print (sliceGrid $ iterReplGrid $ parseStringGrid $ replaceGrid grid index 25)
  playGridTwo (replaceGrid grid index 25)

playGridTwo :: [Integer] -> IO a
playGridTwo grid = do
  --if ()  
  input <- getLine
  let index = (read input :: Int)
  mapM_ print (sliceGrid $ iterReplGrid $ parseStringGrid $ replaceGrid grid index 1)
  playGrid (replaceGrid grid index 1)

main :: IO ()
main = do
  putStrLn "Tic-Tac-Toe!!!"

  let xgrid = initGrid
  mapM_ print (sliceGrid $ iterReplGrid $ parseStringGrid $ xgrid)
  
  input <- getLine
  let index = (read input :: Int)
  mapM_ print (sliceGrid $ iterReplGrid $ parseStringGrid $ replaceGrid xgrid index 25)
  playGridTwo (replaceGrid xgrid index 25)

  return()
