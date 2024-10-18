module Init where

import Control.Monad.State
import System.Random
import Types

-- | Initialize a single cell with a random character and zero intensity
initCell :: State StdGen Cell
initCell = do
  charIdx <- state $ randomR (0, length chars - 1)
  return $ Cell charIdx 0

-- | Initialize a column of cells with the given height
initColumn :: Int -> State StdGen Column
initColumn h = replicateM h initCell

-- | Initialize the entire matrix with the given width and height
initMatrix :: Int -> Int -> State StdGen Matrix
initMatrix w h = replicateM w (initColumn h)
