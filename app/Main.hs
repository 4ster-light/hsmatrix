module Main where

import Control.Concurrent
import Control.Monad
import Data.List ()
import System.Console.ANSI
import qualified System.Console.Terminal.Size as TS
import System.IO (hFlush, stdout)
import System.Random

-- Data types
data Cell = Cell
  { charIndex :: Int,
    intensity :: Int -- 0: Inactive, 1-5: Active (5 being the brightest)
  }
  deriving (Eq)

type Column = [Cell]

type Matrix = [Column]

-- Constants
chars :: String
chars = ['ｱ', 'ｲ', 'ｳ', 'ｴ', 'ｵ', 'ｶ', 'ｷ', 'ｸ', 'ｹ', 'ｺ', 'ｻ', 'ｼ', 'ｽ', 'ｾ', 'ｿ', 'ﾀ', 'ﾁ', 'ﾂ', 'ﾃ', 'ﾄ', 'ﾅ', 'ﾆ', 'ﾇ', 'ﾈ', 'ﾉ', 'ﾊ', 'ﾋ', 'ﾌ', 'ﾍ', 'ﾎ', 'ﾏ', 'ﾐ', 'ﾑ', 'ﾒ', 'ﾓ', 'ﾔ', 'ﾕ', 'ﾖ', 'ﾗ', 'ﾘ', 'ﾙ', 'ﾚ', 'ﾛ', 'ﾜ', 'ﾝ']

-- Initialize a random cell
initCell :: IO Cell
initCell = do
  charIdx <- randomRIO (0, length chars - 1)
  return $ Cell charIdx 0

-- Initialize a random column
initColumn :: Int -> IO Column
initColumn h = replicateM h initCell

-- Initialize the matrix
initMatrix :: Int -> Int -> IO Matrix
initMatrix w h = replicateM w (initColumn h)

-- Update a single column
updateColumn :: Column -> IO Column
updateColumn col = do
  shouldStartNewRain <- (== 0) <$> randomRIO (0, 15 :: Int)
  newCharIdx <- randomRIO (0, length chars - 1)
  let newHead =
        if shouldStartNewRain && intensity (head col) == 0
          then Cell newCharIdx 5
          else head col
  let updatedCol = newHead : init col
  mapM updateCell updatedCol

-- Update a single cell
updateCell :: Cell -> IO Cell
updateCell (Cell idx i) = do
  newCharIdx <- randomRIO (0, length chars - 1)
  let newIntensity = max 0 (i - 1)
  return $ Cell (if i > 0 then newCharIdx else idx) newIntensity

-- Update the entire matrix
updateMatrix :: Matrix -> IO Matrix
updateMatrix = mapM updateColumn

-- Render a single cell
renderCell :: Cell -> (Char, Color)
renderCell (Cell idx i)
  | i == 5 = (chars !! idx, White)
  | i > 0 = (chars !! idx, Green)
  | otherwise = (' ', Black)

-- Define custom color for fading effect
fadeColor :: Int -> Color
fadeColor i
  | i == 0 = Black
  | i == 1 = Blue
  | i == 2 = Cyan
  | i == 3 = Green
  | i == 4 = Yellow
  | otherwise = White

-- Render the entire matrix
renderMatrix :: Int -> Int -> Matrix -> IO ()
renderMatrix w h matrix = do
  setCursorPosition 0 0
  forM_ [0 .. h - 1] $ \y -> do
    forM_ [0 .. w - 1] $ \x -> do
      let cell = matrix !! x !! y
      let (char, _color) = renderCell cell
      setSGR [SetColor Foreground Vivid (fadeColor (intensity cell))]
      putChar char
    putChar '\n' -- Move to the next line after each row
  hFlush stdout

-- Get terminal size
getCurrentTerminalSize :: IO (Int, Int)
getCurrentTerminalSize = do
  maybeSize <- TS.size
  case maybeSize of
    Just (TS.Window rows cols) -> return (cols, rows - 1) -- Subtract 1 from rows to prevent scrolling
    Nothing -> return (80, 24) -- Default size if unable to get terminal size

-- Adjust matrix size
adjustMatrixSize :: Int -> Int -> Matrix -> IO Matrix
adjustMatrixSize w h matrix = do
  let currentW = length matrix
  -- let currentH = if null matrix then 0 else length (head matrix)

  newMatrix <-
    if currentW /= w
      then do
        let commonCols = min currentW w
        let existingCols = take commonCols matrix
        newCols <- replicateM (max 0 (w - currentW)) (initColumn h)
        return $ existingCols ++ newCols
      else return matrix

  return $ map (take h . (\col -> col ++ repeat (Cell 0 0))) newMatrix

-- Main loop
mainLoop :: Matrix -> IO ()
mainLoop matrix = do
  (w, h) <- getCurrentTerminalSize
  clearScreen
  adjustedMatrix <- adjustMatrixSize w h matrix
  newMatrix <- updateMatrix adjustedMatrix
  renderMatrix w h newMatrix
  threadDelay 50000 -- 50ms delay
  mainLoop newMatrix

-- Main function
main :: IO ()
main = do
  hideCursor
  clearScreen
  (w, h) <- getCurrentTerminalSize
  matrix <- initMatrix w h
  mainLoop matrix
