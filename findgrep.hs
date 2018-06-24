-- Utilite for implement
-- grep . -name "*fileNameSuffix" -print0 | xargs -0 grep -n "strPattern"

import Control.Monad (forM)
import Data.List (isInfixOf, isSuffixOf)
import System.Directory (getDirectoryContents, getCurrentDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.IO (readFile)
import System.FilePath ((</>))

main = do
    [strPattern, fileNameSuffix] <- getArgs
    dir <- getCurrentDirectory
    files <- findRecursiveContents dir fileNameSuffix
    forM files (\name ->  
        doGrepFile strPattern name)

findRecursiveContents :: FilePath -> String -> IO [FilePath]
findRecursiveContents topdir searchName = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames (\name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then findRecursiveContents path searchName
      else 
        if searchName `isSuffixOf` name 
           then return [path]
           else return []
    )
  return (concat paths)

doGrepFile pattern fname = do
    contents <- readFile fname
    putStr (grep pattern contents fname)

grep :: String -> String -> String -> String
grep pattern strings prefix = 
    let allLines = lines strings
        numbered = zip [1..] allLines
        found = filter (\(num, inStr) ->  pattern `isInfixOf` inStr) numbered
        result = map (\(num, str) -> concat [prefix, " ", show num, ": ", str]) found
    in unlines result


