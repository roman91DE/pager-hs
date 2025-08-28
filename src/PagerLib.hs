module PagerLib (
    PagerState(PagerState),
      getInputFile,
      getTerminalSize,
      wrapText,
      parts,
      pagerLoop
 )where

import Control.Exception (SomeException, catch)
import System.Process (readProcess)

data TerminalSize = TerminalSize
  { rows :: Int,
    cols :: Int
  }
  deriving (Show)

data PagerState = PagerState
  { txt :: [[String]],
    idx :: Int
  } deriving (Show)

data UserInput = Next | Prev | Quit deriving (Show)

wrapLine :: TerminalSize -> String -> [String]
wrapLine _ "" = []
wrapLine ts@(TerminalSize _ ncols) s
  | length s < ncols = [s]
  | otherwise =
      let (l, rest) = splitAt ncols s in l : wrapLine ts rest

wrapText :: TerminalSize -> String -> [String]
wrapText ts text = flattened
  where
    lines' = lines text
    wrapped = map (wrapLine ts) lines'
    flattened = concat wrapped

parts :: TerminalSize -> [String] -> [[String]]
parts _ [] = []
parts ts@(TerminalSize nrows _) xs = take nrows xs : parts ts (drop nrows xs)

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
          return $ read io - 1



displayPart' :: PagerState -> IO ()
displayPart' (PagerState chunks idx') = do
  mapM_ putStrLn (chunks !! idx')

getInputFile :: FilePath -> IO String
getInputFile = readFile

pagerLoop :: PagerState -> IO ()
pagerLoop ps@(PagerState chunks idx') = do
    displayPart' ps
    input <- readUser
    case input of
        Next -> if idx' < length chunks-1 then pagerLoop $ PagerState chunks (idx'+1) else pagerLoop ps
        Prev -> if idx' > 0 then pagerLoop $ PagerState chunks (idx'-1) else pagerLoop ps
        Quit -> return ()

readUser :: IO UserInput
readUser = do
    c <- getChar
    case c of
        'n' -> return Next
        'p' -> return Prev
        'q' -> return Quit
        _ -> readUser

