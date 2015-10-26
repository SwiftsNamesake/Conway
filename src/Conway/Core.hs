-- |
-- Module      : Conway.Core
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 25 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Conway.Core where
-- module Conway.Core () where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Maybe   (fromMaybe, catMaybes)
import Data.List    (transpose)
import Data.Functor ((<$>))

import qualified Data.QuadTree as QT

import Conway.Core.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
simulate :: Universe -> [Universe]
simulate = iterate tick


-- | The number of living cells adjacent to each cell (between 0 and 8)
-- TODO: Rewrite the gobbledygook above
neighbours :: Integral n => Universe -> Neighbours n
neighbours uni@(Universe u) = Neighbours $ walk (\cl rw cell -> (fromIntegral . nliving $ adjacent uni cl rw, cell)) u
  where
    nliving = length . filter (==Alive)


-- |
adjacent :: Universe -> Int -> Int -> [Cell]
adjacent uni cl rw = catMaybes $ map (uncurry (at uni) . absolute) [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
  where
    absolute (dx, dy) = (cl+dx, rw+dy)


-- | Traverses the universe lattice
-- TODO: Flesh out description
-- TODO: Rename (?)
-- TODO: Refactor
-- TODO: Decide on order of traversal (eg. row major or column major)
-- traverse
walk :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
walk f grid = [ zipWith (f cl) [0..] column | (cl, column) <- zip [0..] grid]


-- |
at :: Universe -> Int -> Int -> Maybe Cell
at (Universe u) cl rw = (u !? cl) >>= (!? rw)
  where
    (!?) []     _ = Nothing
    (!?) (x:xs) 0 = Just x
    (!?) (_:xs) i = (!?) xs (i-1)


-- | Advances the universe to the next generation
tick :: Universe -> Universe
tick (Universe u) = Universe $ walk (\_ _ -> uncurry survives) ns -- map (map isalive) (neighbours uni)
  where
    (Neighbours ns) = neighbours (Universe u)


-- | Should the cell be alive or dead in the next generation?
--   Rules taken from Wikipedia.
-- TODO: Rename (?)
survives :: Integral n => n -> Cell -> Cell
survives n
  | n < 2  = const Dead  -- Any live cell with fewer than two live neighbours dies, as if caused by under-population.
  | n == 2 = id          -- Any live cell with two
  | n == 3 = const Alive -- or three live neighbours lives on to the next generation. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
  | n > 3  = const Dead  -- Any live cell with more than three live neighbours dies, as if by over-population.


-- |
someFunc :: IO ()
someFunc = putStrLn "someFunc"
