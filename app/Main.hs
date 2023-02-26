{- ======================================= IMPORTS ======================================= -}
import Interactive
import Tools
import System.IO

-- https://hackage.haskell.org/package/filepath-1.4.100.1/docs/System-FilePath-Posix.html
import System.FilePath 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as Map

punctuation = "!\"#$%&'()*+, -./:;<=>?@[]^_`{|}~ "

-- backlog of possible TODOS: ( who has time for this LOL? )
-- PERFORM VALIDATION
-- FILE IS NOT EMPTY, what should happen?
-- FILE EXISTS, read the test,

{- ======================================== MAIN ======================================== -}


main :: IO ()
main 
    = do 
        -- create arraypath of each file.txt in directory
        -- trainPositiveFilePaths <- getAllTxtFilePaths trainPositivePath :: IO [String]
        -- trainNegativeFilePaths <-getAllTxtFilePaths trainNegativePath :: IO [String]
        -- testPositiveFilePaths <- getAllTxtFilePaths testPositivePath :: IO [String] 
        -- testNegativeFilePaths <-getAllTxtFilePaths testNegativePath :: IO [String]
        -- printDirContents trainPositiveFilePaths -- print contents of each file

        -- get the array of stop words to remove
        -- s <- readFileStrict stopwordsFilePath :: IO String
        -- let stopWords = splitOn ['\n'] s :: [String]

        -- comment all out if you don't want the program to take forever
        -- map_train_pos <- getDirContents Map.empty stopWords trainPositiveFilePaths :: IO (Map String Int)
        -- map_train_neg <- getDirContents Map.empty stopWords trainNegativeFilePaths :: IO (Map String Int)
        -- map_test_pos <- getDirContents Map.empty stopWords testPositiveFilePaths :: IO (Map String Int)
        -- map_test_neg <- getDirContents Map.empty stopWords testNegativeFilePaths :: IO (Map String Int)

        -- comment below out if you want
        -- let fileName = "example"
        -- writeMapModelToDiskAsJSON fileName map_train_pos
        -- s <- readJsonModelFromDisk fileName
        -- printMap (Map.assocs s)
        start

        putStr "" -- prevent errors in case no IO is performed 




-- Get directory contents as array of string, each element is an individual review from some txt file from filePaths
getDirContents :: Map String Int -> [String] -> [String] -> IO (Map String Int)
getDirContents dict stopWorsds [] = return dict
getDirContents dict stopWords filePaths@(x:xs)
    = do
        xData <- readFileStrict x

        -- remove all non unicode characters and convert to lowercase 
        let xSplitLower = map (\s -> lowerString (filter keepletter s)) (splitOn punctuation xData) :: [String]
        -- remove stop words
        let xSplitLowerStopWords = filter (`notElem` stopWords) xSplitLower :: [String]
        -- insert the words from the current file x into the dictionary dict
        getDirContents (insertWords dict xSplitLowerStopWords) stopWords xs



insertWords :: Map String Int -> [String] -> Map String Int
insertWords map [] = map
insertWords map words@(x:xs)
    = case Map.lookup x map of
            Just v -> insertWords (Map.insert x (v + 1) map) xs
            Nothing -> insertWords (Map.insert x 1 map) xs



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


-- Helper function to call printFileContents on each path in filePaths 
printDirContents :: [String] -> IO ()
printDirContents [] = putStr ""
printDirContents filePaths@(x:xs)
    = do
        printFileContents x
        printDirContents xs
-- Try in main:
-- txtFilePaths <- getAllTxtFilePaths trainPositivePath :: IO [String] -- create arraypath of each file
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


printMap :: [(String, Int)] -> IO ()
printMap [] = putStr ""
printMap kvpair@((a, b):xs)
    = do
        putStrLn (a ++ " : " ++ show b)
        printMap xs