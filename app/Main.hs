{- ======================================= IMPORTS ======================================= -}
module Main where
import System.IO

-- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html#v%3agetDirectoryContents
import System.Directory 

-- https://hackage.haskell.org/package/filepath-1.4.100.1/docs/System-FilePath-Posix.html
import System.FilePath 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


{- ====================================== CONSTANTS ====================================== -}
datasetsPath = "../datasets" :: FilePath
trainPositivePath = datasetsPath ++ "/train_positive" :: FilePath
trainNegativePath = datasetsPath ++ "/train_negative" :: FilePath
testPositivePath = datasetsPath ++ "/test_positive" :: FilePath
testNegativePath = datasetsPath ++ "/test_negative" :: FilePath

-- exampleFilePath = datasetsPath ++ "/exampleDir"




-- backlog of possible TODOS: ( who has time for this LOL? )
-- PERFORM VALIDATION
-- FILE IS NOT EMPTY, what should happen?
-- FILE EXISTS, read the test,


{- ======================================== MAIN ======================================== -}
main :: IO ()
main 
    = do 
        
        txtFilePaths <- getAllFilePaths trainPositivePath :: IO [String] -- create arraypath of each file
        -- printDirContents txtFilePaths -- print contents of each file
        
        
        
        putStr "" -- prevent errors in case no IO is performed 






-- Get directory contents as array of string, each element is an individual review from some txt file from filePaths
getDirContents :: [String] -> [String] -> IO [String]
getDirContents acc [] = return acc 
getDirContents acc filePaths@(x:xs)
    = do
        xData <- readFileStrict x
        getDirContents (xData : acc) xs

{-
I wanted to keep using hGetContents like in printFileContents, but I got the Error: "hGetContents": illegal operation (delayed read on closed handle)
apparently it's because hGetContents is lazily evaluated

So I did this:
    getFileContents :: FilePath -> IO String
    getFileContents filePath = do readFile filePath 

but then I got the Error: "getCurrentDirectory: resource exhausted (Too many open files)" error
This is apparently because Prelude.readFile has lazy I/O, but pure I/O is: using Data.Text.IO.readFile
https://stackoverflow.com/a/22894297/11792937
-}
readFileStrict :: FilePath -> IO String
readFileStrict = fmap T.unpack . TIO.readFile
-- Try in main:
-- filedataTestPos_0_10_txt <- readFileStrict (testPositivePath ++ "/" ++ "0_10.txt")
-- putStrLn filedataTestPos_0_10_txt


{- ======================================= HELPERS ======================================= -}

-- Individually perform putStrLn on array of string.
-- Helpful to see what's happening
printArray :: [String] -> IO ()
printArray [] = putStr ""
printArray arr@(x:xs) 
    = do
        putStrLn (x ++ "\n")
        printArray xs
-- Try in main:
-- txtFilePaths <- getAllFilePaths trainPositivePath :: IO [String] 
-- content <- getDirContents [] txtFilePaths
-- printArray content


-- Helper function to call printFileContents on each path in filePaths 
printDirContents :: [String] -> IO ()
printDirContents [] = putStr ""
printDirContents filePaths@(x:xs)
    = do
        printFileContents x
        printDirContents xs
-- Try in main:
-- txtFilePaths <- getAllFilePaths trainPositivePath :: IO [String] -- create arraypath of each file
-- printDirContents txtFilePaths -- print contents of each file


-- Possible errors:
-- the openFile computation may fail with isAlreadyInUseError if the file is already open and cannot be reopened; 
-- isDoesNotExistError if the file does not exist; or isPermissionError if the user does not have permission to open the file.
printFileContents :: FilePath -> IO ()
printFileContents filePath 
    = do
        -- openFile allocates and returns a new open handle to manage the file in filePath
        -- Opening in ReadMode, the handle is positioned at the beginning
        hdl <- openFile filePath ReadMode
        contents <- hGetContents hdl
        putStrLn contents -- returns the list of characters corresponding to the unread portion of the channel or file managed by hdl
        
        hClose hdl -- Closing the file to avoid causing isAlreadyInUseError in the future



-- for a directory path dirPath, get all the files with .txt extension, and concatenate the dirPath + '/' file.txt to get the file path relative to directory
getAllFilePaths dirPath 
    = do
        dirContents <- getDirectoryContents dirPath
        let txtFilePaths = map (\x -> dirPath ++ "/" ++ x) (filter isTxtFile dirContents)
        return txtFilePaths



-- returns true if file is a txt file else false
isTxtFile :: FilePath -> Bool
isTxtFile file = takeExtension file == ".txt"



-- returns the number of items in a foldable t
countList:: [t] -> Integer 
countList [] = 0
countList (x:xs) = 1 + countList xs
-- Try:
-- countList ['a', '2']