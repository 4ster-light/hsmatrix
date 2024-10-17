module Main where

import Control.Concurrent
import Control.Monad
import Data.List ()
import System.Console.ANSI
import qualified System.Console.Terminal.Size as TS
import System.IO (hFlush, stdout)
import System.Random

data Cell = Cell
  { charIndex :: Int,
    intensity :: Int -- 0: Inactive, 1-5: Active (5 being the brightest)
  }
  deriving (Eq)

type Column = [Cell]

type Matrix = [Column]

chars :: String
chars = ['ｱ', 'ｲ', 'ｳ', 'ｴ', 'ｵ', 'ｶ', 'ｷ', 'ｸ', 'ｹ', 'ｺ', 'ｻ', 'ｼ', 'ｽ', 'ｾ', 'ｿ', 'ﾀ', 'ﾁ', 'ﾂ', 'ﾃ', 'ﾄ', 'ﾅ', 'ﾆ', 'ﾇ', 'ﾈ', 'ﾉ', 'ﾊ', 'ﾋ', 'ﾌ', 'ﾍ', 'ﾎ', 'ﾏ', 'ﾐ', 'ﾑ', 'ﾒ', 'ﾓ', 'ﾔ', 'ﾕ', 'ﾖ', 'ﾗ', 'ﾘ', 'ﾙ', 'ﾚ', 'ﾛ', 'ﾜ', 'ﾝ']

initCell :: IO Cell
initCell = do
  charIdx <- randomRIO (0, length chars - 1)
  return $ Cell charIdx 0

initColumn :: Int -> IO Column
initColumn h = replicateM h initCell

initMatrix :: Int -> Int -> IO Matrix
initMatrix w h = replicateM w (initColumn h)

-- Update a single column
updateColumn :: Int -> Column -> IO Column
updateColumn h col = do
  shouldStartNewRain <- (== 0) <$> randomRIO (0, 20 :: Int)
  newCharIdx <- randomRIO (0, length chars - 1)
  let rainLength = randomRIO (5, 15) >>= \len -> return (min len h)
  let activeLength = length $ takeWhile (\c -> intensity c > 0) col
  newHead <- if shouldStartNewRain && activeLength == 0
             then do
               len <- rainLength
               return $ replicate len (Cell newCharIdx 5) ++ replicate (h - len) (Cell 0 0)
             else return [Cell 0 0]
  return $ take h $ newHead ++ col

-- Update a single cell
updateCell :: Cell -> Cell
updateCell (Cell idx i)
  | i > 0 = Cell idx (max 1 (i - 1))
  | otherwise = Cell idx 0

-- Update the entire matrix
updateMatrix :: Int -> Matrix -> IO Matrix
updateMatrix h = mapM (updateColumn h)

-- Define custom color for fading effect
fadeColor :: Int -> Color
fadeColor i
  | i == 5 = White
  | i > 0 = Green
  | otherwise = Black

-- Render the entire matrix
renderMatrix :: Int -> Int -> Matrix -> IO ()
renderMatrix w h matrix = do
  setCursorPosition 0 0
  forM_ [0 .. h - 1] $ \y -> do
    forM_ [0 .. w - 1] $ \x -> do
      let cell = matrix !! x !! y
      setSGR [SetColor Foreground Vivid (fadeColor (intensity cell))]
      if intensity cell > 0
        then putChar (chars !! charIndex cell)
        else putChar ' '
    putChar '\n' -- Move to the next line after each row
  hFlush stdout

-- Get terminal size
getCurrentTerminalSize :: IO (Int, Int)
getCurrentTerminalSize = do
  maybeSize <- TS.size
  case maybeSize of
    Just (TS.Window rows cols) -> return (cols, rows - 1) -- Subtract 1 from rows to prevent scrolling
    Nothing -> return (80, 24) -- Default size if unable to get terminal size

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

  -- Adjust the height of each column
  return $ map (adjustColumnHeight h) newMatrix
  where
    adjustColumnHeight :: Int -> Column -> Column
    adjustColumnHeight targetH col
      | length col < targetH = col ++ replicate (targetH - length col) (Cell 0 0)
      | length col > targetH = take targetH col
      | otherwise = col

mainLoop :: Matrix -> IO ()
mainLoop matrix = do
  (w, h) <- getCurrentTerminalSize
  adjustedMatrix <- adjustMatrixSize w h matrix
  newMatrix <- updateMatrix h adjustedMatrix
  renderMatrix w h (map (map updateCell) newMatrix)
  threadDelay 50000 -- 50ms delay
  mainLoop newMatrix

main :: IO ()
main = do
  hideCursor
  clearScreen
  (w, h) <- getCurrentTerminalSize
  matrix <- initMatrix w h
  mainLoop matrix
