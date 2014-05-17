-- file: omni.hs

import Control.Applicative   ((<$>), (<*>))
import Data.List             (intercalate)
import System.IO             (readFile)

import Data.Map.Strict       (Map, (!), member, empty, singleton, union, toList)
import Network.HTTP.Base     (urlEncode)
import System.Directory      (getHomeDirectory, doesFileExist,
                              doesDirectoryExist)
import System.FilePath.Posix (combine)
import System.Process        (createProcess, shell)
import Text.CSV              (Record, parseCSV)
import Text.Regex.Posix      ((=~))

searchEngineFile :: IO String
searchEngineFile = getHomeDirectory >>= return . (\d -> combine d ".omnirc")
searchDelimeter :: String
searchDelimeter  = "%s"

main :: IO ()
main = do
  (t:r) <- arguments <$> getLine
  f <- searchEngineFile >>= readFile
  let ses = parseSearchEngines f
  inFS <- (||) <$> (doesFileExist t)
               <*> (doesDirectoryExist t)
  resolve t (intercalate " " r) ses inFS
  

resolve :: String -> String -> Map String (String -> String) -> Bool
           -> IO ()
resolve t r ses inFS
  | t == "x"       = exec r
  | t `member` ses = open $ (ses ! t) r
  | inFS           = open t
  | isURL t        = open t 
  | otherwise      = error "Bad input"

parseSearchEngines :: String -> Map String (String -> String)
parseSearchEngines f =
  case parseCSV "" cleanF of
    Left  _  -> error "Bad file"
    Right rs -> foldl union empty $ map toEngine rs
  where cleanF = unlines . filter ((/= '#') . head) . lines $ f

toEngine :: Record -> Map String (String -> String)
toEngine (k:v:[]) = singleton k $ toFun v
  where
    toFun :: String -> (String -> String)
    toFun s q = case (s =~ searchDelimeter :: (String,String,String)) of
      (b,d,a) -> b ++ (urlEncode q) ++ a
toEngine _ = empty

-- TODO, improve URL matching
isURL :: String -> Bool
isURL = (=~ ".*\\..*")

-- TODO, cleanup (possibly find a library function)
arguments :: String -> [String]
arguments "" = [] 
arguments (' ':s) = arguments s
arguments ('\"':s) = ("\"" ++ word ++ "\""):(arguments rest)
  where (word,('\"':rest)) = break (\c -> c == '\"') s
arguments s = word:(arguments rest)
  where
    (word,rest) = break (\c -> c == ' ' || c == '\"') s

exec :: String -> IO ()
exec c = createProcess (shell c) >> return ()

open :: String -> IO ()
open = exec . ("xdg-open " ++)
