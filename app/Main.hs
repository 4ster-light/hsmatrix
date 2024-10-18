module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.State
import Data.List (transpose)
import System.Console.ANSI
import qualified System.Console.Terminal.Size as TS
import System.IO (hFlush, stdout)
import System.Random

import Init
import Types
import Update

-- | Determine the color of a cell based on its intensity
fadeColor :: Int -> Color
fadeColor i
  | i == 5 = White
  | i > 0 = Green
  | otherwise = Black

-- | Render the matrix to the terminal
renderMatrix :: Matrix -> IO ()
renderMatrix matrix = do
  setCursorPosition 0 0
  forM_ (transpose matrix) $ \row -> do
    forM_ row $ \cell -> do
      setSGR [SetColor Foreground Vivid (fadeColor (intensity cell))]
      putChar $ if intensity cell > 0 then chars !! charIndex cell else ' '
    putChar '\n'
  hFlush stdout

-- | Get the current terminal size
getCurrentTerminalSize :: IO (Int, Int)
getCurrentTerminalSize = do
  maybeSize <- TS.size
  case maybeSize of
    Just (TS.Window rows cols) -> return (cols, rows - 1)
    Nothing -> return (80, 24) -- Default size if unable to determine

-- | Main loop for updating and rendering the matrix
mainLoop :: Matrix -> StdGen -> IO ()
mainLoop matrix gen = do
  (w, h) <- getCurrentTerminalSize
  let (adjustedMatrix, gen1) = runState (adjustMatrixSize w h matrix) gen
  let (newMatrix, gen2) = runState (updateMatrix h adjustedMatrix) gen1
  let updatedMatrix = map (map updateCell) newMatrix
  renderMatrix updatedMatrix
  threadDelay 50000 -- 50ms delay
  mainLoop updatedMatrix gen2

-- | Main function to set up and start the matrix rain effect
main :: IO ()
main = do
  hideCursor
  clearScreen
  (w, h) <- getCurrentTerminalSize
  gen <- getStdGen
  let (initialMatrix, initialGen) = runState (initMatrix w h) gen
  mainLoop initialMatrix initialGen
