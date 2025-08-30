module PagerLib
  ( PagerState (PagerState),
    getInputFile,
    getTerminalSize,
    wrapText,
    parts,
    pagerLoop,
    getFilepaths,
    readFiles,
  )
where

import Control.Exception (SomeException, catch)
import System.Environment (getArgs)
import System.IO.Error (tryIOError)
import System.Process (readProcess)

data TerminalSize = TerminalSize
  { rows :: Int,
    cols :: Int
  }
  deriving (Show)

data PagerState = PagerState
  { txt :: [[String]],
    idx :: Int
  }
  deriving (Show)

data UserInput = Next | Prev | Quit deriving (Show)

getFilepaths :: IO (Either String [FilePath])
getFilepaths = do
  args <- getArgs
  case args of
    [] -> return $ Left "USAGE: PAGER FILEPATH (FILEPATH_2) (FILEPATH_N)"
    xs -> return $ Right xs

readFiles :: [FilePath] -> IO (Either String String)
readFiles [] = return $ Left "No files to read"
readFiles fs = do
  results <- mapM readSingleFile fs
  case sequence results of
    Left err -> return $ Left err
    Right contents -> return $ Right (concat contents)
  where
    readSingleFile :: FilePath -> IO (Either String String)
    readSingleFile f = do
      result <- tryIOError (Prelude.readFile f)
      case result of
        Left err -> return $ Left ("Error reading " ++ f ++ ": " ++ show err)
        Right txt' -> return $ Right txt'

wrapLine :: TerminalSize -> String -> [String]
wrapLine _ "" = []
wrapLine (TerminalSize _ ncols) s
  | length s < ncols = [s]
  | otherwise =
      if longestWordLength >= ncols
        then hardWrapLine s
        else map unwords (reverse (map reverse splitted))
  where
    hardWrapLine :: String -> [String]
    hardWrapLine "" = []
    hardWrapLine s' = let (l, rest) = splitAt ncols s' in l : hardWrapLine rest
    words' = words s
    longestWordLength = foldl (\acc x -> max (length x) acc) 0 words'
    splitted = foldl f [[]] words'
    f acc word =
      let currentLine = head acc
          currentLen = length (unwords (reverse currentLine))
       in if currentLen + length word + (if null currentLine then 0 else 1) <= ncols
            then (word : currentLine) : tail acc
            else [word] : acc

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
    Next -> if idx' < length chunks - 1 then pagerLoop $ PagerState chunks (idx' + 1) else pagerLoop ps
    Prev -> if idx' > 0 then pagerLoop $ PagerState chunks (idx' - 1) else pagerLoop ps
    Quit -> return ()

readUser :: IO UserInput
readUser = do
  c <- getChar
  case c of
    'n' -> return Next
    'p' -> return Prev
    'q' -> return Quit
    _ -> readUser
