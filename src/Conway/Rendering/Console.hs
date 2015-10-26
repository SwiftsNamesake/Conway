-- |
-- Module      : Conway.Rendering.Console
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 26 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Conway.Rendering.Console (module Conway.Rendering.Console, module System.Console.ANSI) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.List           (transpose, intersperse, maximumBy)
import Data.Ord            (comparing)
import Control.Concurrent  (threadDelay)

import System.Console.ANSI

import Conway.Core.Types
import Conway.Core



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
animate :: Int -> [Universe] -> IO ()
animate fps frames = mapM_ frame frames
  where
    frame :: Universe -> IO ()
    frame u = do
      putStrLn $ showUniverse u
      wait $ 1.0/fromIntegral fps
      clear u


wait :: (RealFrac n) => n -> IO ()
wait s = threadDelay . floor $ s * 10^6


-- |
clear :: Universe -> IO ()
clear = cursorUp . (+1) . height


-- |
width :: Integral i => Universe -> i
width (Universe u) = fromIntegral $ length u


-- |
height :: Integral i => Universe -> i
height (Universe u) = fromIntegral . length $ maximumBy (comparing length) u


-- |
initial :: Universe
initial = Universe . transpose . map (map parse) $ ["                           ",
                                                    "     O                     ",
                                                    "      O                    ",
                                                    "    OOO                    ",
                                                    "                           ",
                                                    "                           ",
                                                    "                           ",
                                                    "                           "]


-- |
parse :: Char -> Cell
parse 'O' = Alive
parse ' ' = Dead
parse  _  = error "What have you done?"


-- |
tile :: Cell -> Char
tile Alive = 'O'
tile Dead  = ' '


-- |
showGrid :: (a -> Char) -> [[a]] -> [String]
showGrid f grid = walk (\cl rw cell -> f cell) grid


-- |
showUniverse :: Universe -> String
showUniverse (Universe u) = unlines . transpose $ showGrid tile u
