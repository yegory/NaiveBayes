{- ======================================= IMPORTS ======================================= -}
module Main where
import System.IO

-- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html#v%3agetDirectoryContents
import System.Directory 

-- https://hackage.haskell.org/package/filepath-1.4.100.1/docs/System-FilePath-Posix.html
import System.FilePath 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (ord, chr)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isAscii)
{- ====================================== DATA DEF ====================================== -}
datasetsPath = "../datasets" :: FilePath
trainPositivePath   = datasetsPath ++ "/train_positive" :: FilePath
trainNegativePath   = datasetsPath ++ "/train_negative" :: FilePath
testPositivePath    = datasetsPath ++ "/test_positive"  :: FilePath
testNegativePath    = datasetsPath ++ "/test_negative"  :: FilePath
stopwordsFilePath   = datasetsPath ++ "/stopwords.txt"  :: FilePath
exampleDirPath      = datasetsPath ++ "/exampleDir"

punctuation = "!\"#$%&'()*+, -./:;<=>?@[\\]^_`{|}~" 



-- backlog of possible TODOS: ( who has time for this LOL? )
-- PERFORM VALIDATION
-- FILE IS NOT EMPTY, what should happen?
-- FILE EXISTS, read the test,


{- ======================================== MAIN ======================================== -}
main :: IO ()
main 
    = do 
        -- create arraypath of each file.txt in directory
        trainPositiveFilePaths <-getAllFilePaths trainPositivePath :: IO [String]
        trainNegativeFilePaths <-getAllFilePaths trainNegativePath :: IO [String]
        testPositiveFilePaths <- getAllFilePaths testPositivePath :: IO [String] 
        testNegativeFilePaths <-getAllFilePaths testNegativePath :: IO [String]
        -- printDirContents txtFilePaths -- print contents of each file

        -- get the array of stop words to remove
        s <- readFileStrict stopwordsFilePath :: IO String
        let stopWords = splitOn ['\n'] s :: [String]

        -- comment all out if you don't want the program to take forever
        map_train_pos <- getDirContents Map.empty stopWords trainPositiveFilePaths :: IO (Map String Int)
        map_train_neg <- getDirContents Map.empty stopWords trainNegativeFilePaths :: IO (Map String Int)
        map_test_pos <- getDirContents Map.empty stopWords testPositiveFilePaths :: IO (Map String Int)
        map_test_neg <- getDirContents Map.empty stopWords testNegativeFilePaths :: IO (Map String Int)
        -- printMap (Map.assocs map_train_pos)
        -- printMap (Map.assocs map_train_neg)
        -- printMap (Map.assocs map_test_pos)
        -- printMap (Map.assocs map_test_neg)
        -- printStringArray f



        let zorCount = Map.lookup "zor" map_train_pos :: Maybe Int
        let angelsCount = Map.lookup "angels" map_train_pos :: Maybe Int
        putStrLn (show zorCount)
        putStrLn (show angelsCount)
        
        putStr "" -- prevent errors in case no IO is performed 


printMap :: [(String, Int)] -> IO ()
printMap [] = putStr ""
printMap kvpair@((a, b):xs)
    = do
        putStrLn (a ++ " : " ++ show b)
        printMap xs

-- Get directory contents as array of string, each element is an individual review from some txt file from filePaths
getDirContents :: Map String Int -> [String] -> [String] -> IO (Map String Int)
getDirContents dict stopWorsds [] = return dict
getDirContents dict stopWords filePaths@(x:xs)
    = do
        xData <- readFileStrict x

        -- remove all non unicode characters and convert to lowercase 
        let xSplitLower = map (\s -> lowerString (filter isAscii s)) (splitOn punctuation xData) :: [String]
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

-- Return the string representation of str with all lowercase letters
lowerString :: String -> String
lowerString "" = ""
lowerString str@(x:xs) = letterToLower x : lowerString xs 

-- return the lower case letter representation of ch
letterToLower :: Char -> Char
letterToLower ch 
    | isCapitalLetter ch = chr (ord ch + 32)
    | otherwise = ch

-- returns true if the character is a capital letter, false if ch is any other character
isCapitalLetter :: Char -> Bool
isCapitalLetter ch = code >= 65 && code <= 90
                     where code = ord ch

-- Helper function to split fdata (file data) on a given list of separators (delim)
splitOn :: (Eq t) => [t] -> [t] -> [[t]] 
splitOn delim [] = [[]]
splitOn delim fdata@(x:xs)
    | x `elem` delim = [] : splitOn delim xs
    | otherwise = ((x:acc) : tail)
                where acc:tail = splitOn delim xs



-- Individually perform putStrLn on array of string.
-- Helpful to see what's happening
printStringArray :: [String] -> IO ()
printStringArray [] = putStr ""
printStringArray arr@(x:xs) 
    = do
        if x /= ""
            then putStrLn x
            else putStr ""
        printStringArray xs
-- Try in main:
-- txtFilePaths <- getAllFilePaths trainPositivePath :: IO [String] 
-- content <- getDirContents [] txtFilePaths
-- printStringArray content


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