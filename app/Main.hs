module Main where

import PagerLib
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  fileResult <- getFilepaths
  case fileResult of
    Left errmsg -> putStrLn errmsg
    Right fpath -> do
      inputResult <- readFiles fpath
      case inputResult of
        Left err -> putStrLn $ "Error reading file: " ++ err
        Right input -> do
          termSize <- getTerminalSize
          let chunks' = wrapText termSize input
          let parts' = parts termSize chunks'
          let ps = PagerState parts' 0
          pagerLoop ps
