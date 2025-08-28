module PagerLib where

import Control.Exception (SomeException, catch)
import System.Process (readProcess)

data TerminalSize = TerminalSize
  { rows :: Int,
    cols :: Int
  }
  deriving (Show)

getTerminalSize :: IO TerminalSize
getTerminalSize =
  catch getTerminalSize' handler
  where
    handler :: SomeException -> IO TerminalSize
    handler _ = pure (TerminalSize 24 80)
    getTerminalSize' :: IO TerminalSize
    getTerminalSize' = do
      nrows <- callTput "lines"
      ncols <- callTput "cols"
      return $ TerminalSize nrows ncols
      where
        callTput :: String -> IO Int
        callTput s = do
          io <- readProcess "tput" [s] ""
          return $ read io

wrapLine :: TerminalSize -> String -> [String]
wrapLine _ "" = []
wrapLine ts@(TerminalSize _ ncols) s
  | length s < ncols = [s]
  | otherwise =
      let (l, rest) = splitAt ncols s in l : wrapLine ts rest

screenChunks :: TerminalSize -> String -> [String]
screenChunks ts text = flattened
  where
    lines' = lines text
    wrapped = map (wrapLine ts) lines'
    flattened = concat wrapped

parts :: TerminalSize -> [String] -> [[String]]
parts _ [] = []
parts ts@(TerminalSize nrows _) xs = take nrows xs : parts ts (drop nrows xs)

displayPart :: [String] -> IO ()
displayPart xs = do mapM_ putStrLn xs

