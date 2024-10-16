{-# LANGUAGE RecordWildCards #-}

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
    intensity :: Int -- 0: Space, 1-3: Fading, 4: Full intensity
  }
  deriving (Eq)

type Column = [Cell]

type Matrix = [Column]

-- Constants
chars :: String
chars = ['ｱ', 'ｲ', 'ｳ', 'ｴ', 'ｵ', 'ｶ', 'ｷ', 'ｸ', 'ｹ', 'ｺ', 'ｻ', 'ｼ', 'ｽ', 'ｾ', 'ｿ', 'ﾀ', 'ﾁ', 'ﾂ', 'ﾃ', 'ﾄ', 'ﾅ', 'ﾆ', 'ﾇ', 'ﾈ', 'ﾉ', 'ﾊ', 'ﾋ', 'ﾌ', 'ﾍ', 'ﾎ', 'ﾏ', 'ﾐ', 'ﾑ', 'ﾒ', 'ﾓ', 'ﾔ', 'ﾕ', 'ﾖ', 'ﾗ', 'ﾘ', 'ﾙ', 'ﾚ', 'ﾛ', 'ﾜ', 'ﾝ']

-- Initialize a random column
initColumn :: Int -> IO Column
initColumn h = do
  len <- randomRIO (0, h `div` 2)
  replicateM h $ do
    charIdx <- randomRIO (0, length chars - 1)
    intensity <- if len > 0 then randomRIO (1, 4) else return 0
    return $ Cell charIdx intensity

-- Initialize the matrix
initMatrix :: Int -> Int -> IO Matrix
initMatrix w h = replicateM w (initColumn h)

-- Update a single cell
updateCell :: Cell -> IO Cell
updateCell Cell {..} = do
  let newIntensity = max 0 (intensity - 1)
  newIdx <- if newIntensity == 0 then return 0 else randomRIO (0, length chars - 1)
  return $ Cell newIdx newIntensity

-- Update a single column
updateColumn :: Column -> IO Column
updateColumn col = do
  shouldStartNewRain <- (== 0) <$> randomRIO (0, 10 :: Int)

  newHead <-
    if shouldStartNewRain
      then Cell <$> randomRIO (0, length chars - 1) <*> pure 4
      else return (head col)

  let updatedTail = if null col then [] else tail col
  updatedTailCells <- mapM updateCell updatedTail

  return (newHead : updatedTailCells)

-- Update the entire matrix
updateMatrix :: Matrix -> IO Matrix
updateMatrix = mapM updateColumn

-- Render a single cell
renderCell :: Cell -> (Char, Color)
renderCell Cell {..} = (chars !! charIndex, color)
  where
    color = case intensity of
      0 -> Green -- This will render as a space
      4 -> White
      _ -> Green

-- Render the entire matrix
renderMatrix :: Int -> Int -> Matrix -> IO ()
renderMatrix w h matrix = do
  forM_ [0 .. h - 1] $ \y -> do
    forM_ [0 .. w - 1] $ \x -> do
      setCursorPosition y x
      let cell = matrix !! x !! y
      let (char, color) = renderCell cell
      setSGR [SetColor Foreground Vivid color]
      putChar $ if intensity cell == 0 then ' ' else char
  hFlush stdout

-- Main loop
mainLoop :: Int -> Int -> Matrix -> IO ()
mainLoop w h matrix = do
  renderMatrix w h matrix
  newMatrix <- updateMatrix matrix
  threadDelay 50 -- Shorter delay to speed up the "rain"
  mainLoop w h newMatrix

-- Main function
main :: IO ()
main = do
  hideCursor
  clearScreen

  -- Get terminal size
  maybeSize <- TS.size
  (w, h) <- case maybeSize of
    Just (TS.Window rows cols) -> return (cols, rows)
    Nothing -> return (80, 24) -- Default size if unable to get terminal size
  matrix <- initMatrix w h
  mainLoop w h matrix
