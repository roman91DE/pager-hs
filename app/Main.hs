module Main where

import PagerLib
import System.IO (hSetBuffering, hSetEcho, stdin, BufferMode(NoBuffering))



main :: IO ()
main = do
  hSetBuffering stdin NoBuffering 
  hSetEcho stdin False
  input <- getInputFile "n.cabal"
  termSize <- getTerminalSize
  let chunks' = wrapText termSize input
  let parts' = parts termSize chunks'
  let ps = PagerState parts' 0
  pagerLoop ps


