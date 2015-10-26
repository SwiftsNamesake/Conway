-- |
-- Module      : Main
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 25 2015



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Main where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Text.Printf

import Conway.Core
import Conway.Rendering.Console



--------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do
  setTitle "Conway's Game of Life"
  animate 3 . take 10 $ simulate initial
  mapM putStrLn . replicate (height initial) . replicate 30 $ ' '
  cursorUp (height initial)
  putStrLn . unlines . map (centre ' ' 30) $ ["",
                                              "THANKS FOR WATCHING",
                                              "",
                                              "Conway's Game of Life",
                                              "by",
                                              "J. H. Sundqvist"]
  wait 4.5
  cursorUp (7)
  mapM putStrLn . replicate 8 . replicate 30 $ ' '
  cursorUp (7)
  putStrLn $ centre ' ' 30 "GOOD BYE"
  cursorDown $ 7
  where
    centre c ln t = let d = fromIntegral (ln - length t)/2 in replicate (floor d) c ++ t ++ replicate (ceiling d) c
