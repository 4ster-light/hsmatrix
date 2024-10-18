module Update where

import Control.Monad.State
import Init (initColumn)
import System.Random
import Types

-- | Update a single cell by decreasing its intensity
updateCell :: Cell -> Cell
updateCell (Cell idx i)
  | i > 0 = Cell idx (max 1 (i - 1))
  | otherwise = Cell idx 0

-- | Update a column, potentially starting a new rain drop
updateColumn :: Int -> Column -> State StdGen Column
updateColumn h col = do
  shouldStartNewRain <- state $ randomR (0, 20 :: Int)
  newCharIdx <- state $ randomR (0, length chars - 1)
  rainLength <- state $ randomR (5, 15)
  let activeLength = length $ takeWhile (\c -> intensity c > 0) col
  let newHead =
        if shouldStartNewRain == 0 && activeLength == 0
          then replicate (min rainLength h) (Cell newCharIdx 5) ++ replicate (h - min rainLength h) (Cell 0 0)
          else [Cell 0 0]
  return $ take h $ newHead ++ col

-- | Update the entire matrix
updateMatrix :: Int -> Matrix -> State StdGen Matrix
updateMatrix h = mapM (updateColumn h)

-- | Adjust the matrix size to match the current terminal size
adjustMatrixSize :: Int -> Int -> Matrix -> State StdGen Matrix
adjustMatrixSize w h matrix = do
  let currentW = length matrix
  newCols <- replicateM (max 0 (w - currentW)) (initColumn h)
  let adjustedMatrix = take w $ matrix ++ newCols
  return $ map (adjustColumnHeight h) adjustedMatrix
  where
    adjustColumnHeight targetH col
      | length col < targetH = col ++ replicate (targetH - length col) (Cell 0 0)
      | length col > targetH = take targetH col
      | otherwise = col
