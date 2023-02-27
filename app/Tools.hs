module Tools where

import Data.Char (ord, chr)
import System.FilePath (takeExtension, takeBaseName)
import Data.List (sort)
-- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html#v%3agetDirectoryContents
import System.Directory 
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
{- ====================================== DATA DEF ====================================== -}

datasetsPath = "datasets_dev" :: FilePath
modelsPath = "../models" :: FilePath

trainPositivePath   = datasetsPath ++ "/train_positive" :: FilePath
trainNegativePath   = datasetsPath ++ "/train_negative" :: FilePath
testPositivePath    = datasetsPath ++ "/test_positive"  :: FilePath
testNegativePath    = datasetsPath ++ "/test_negative"  :: FilePath
stopwordsFilePath   = datasetsPath ++ "/stopwords.txt"  :: FilePath
exampleDirPath      = datasetsPath ++ "/exampleDir"



-- remove whitespace left of the leftmost character, and right of the rightmost character, and collapse all inner spaces into one
-- credit to D. Poole 2023, the line <"trim (h:' ':' ':r) = trim (h : ' ' : r)"> was too difficult to figure out on my own
trim :: String -> String
trim "" = ""
trim (' ':r) = trim r
trim (h:" ") = [h]
trim (h:' ':' ':r) = trim (h : ' ' : r) 
trim (h:' ':r) = h : ' ' : trim r
trim [h] = [h]
trim (h:r) = h : trim r 
    


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

keepletter :: Char -> Bool
keepletter ch = code >= 97 && code <= 122
                     where code = ord ch


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
-- Try in Main:
-- txtFilePaths <- getAllFilePaths trainPositivePath :: IO [String] 
-- content <- getDirContents [] txtFilePaths
-- printStringArray content

-- Helper function to split fdata (file data) on a given list of separators (delim)
splitOn :: (Eq t) => [t] -> [t] -> [[t]] 
splitOn delim [] = [[]]
splitOn delim fdata@(x:xs)
    | x `elem` delim = [] : splitOn delim xs
    | otherwise = ((x:acc) : tail)
                where acc:tail = splitOn delim xs



-- returns true if file is a txt file else false
isTxtFile :: FilePath -> Bool
isTxtFile file = isExtension file ".txt" 
-- Try:
-- isTxtFile "" => False
-- isTxtFile ".txt" => True
-- isTxtFile ".TXT" => True
-- isTxtFile "file" => False
-- isTxtFile "file.txt" => True
-- isTxtFile "file.txtt => False
-- isTxtFile "file.txt.txtt" => False
-- isTxtFile "file with space.txt" => True

isJsonFile :: FilePath -> Bool
isJsonFile file = isExtension file ".json"
-- Similar tests as for isTxtFile

isExtension :: FilePath -> String -> Bool
isExtension file ext = lowerString (takeExtension file) == ext
-- Try:
-- isExtension "./directory/random/path/file.ext" ".txt" => False
-- isExtension "./directory/random/path/file.txt" ".txt" => True
-- isExtension "./directory/random/path/file.jSoN" ".json" => True


-- for a directory path dirPath, get all the files with .txt extension, and concatenate the dirPath + '/' file.txt to get the file path relative to directory
getAllTxtFilePaths :: FilePath -> IO [FilePath]
getAllTxtFilePaths dirPath 
    = do
        dirContents <- getDirectoryContents dirPath :: IO [FilePath]
        return (map (\x -> dirPath ++ "/" ++ x) (filter isTxtFile dirContents))

-- for a directory path dirPath, get all the files with .json extension, and concatenate the dirPath + '/' file.json to get the file path relative to directory
getAllJsonFilePaths :: FilePath -> IO [FilePath]
getAllJsonFilePaths dirPath 
    = do
        dirContents <- getDirectoryContents dirPath :: IO [FilePath]
        return (map (\x -> dirPath ++ "/" ++ x) (filter isJsonFile dirContents))

-- Assumes models path exists
getAllJsonFileNames :: IO [String]
getAllJsonFileNames 
    = do
        dirContents <- getDirectoryContents modelsPath
        return $ sort (map takeBaseName (filter isJsonFile dirContents))

readJsonModelFromDisk :: FilePath -> IO (Map String Int)
readJsonModelFromDisk fileName
    = do
        let filePath = modelsPath ++ "/" ++ fileName ++ ".json"
        let byteStringData = BS.readFile filePath
        decodedMap <- fmap Aeson.decode byteStringData
        case decodedMap of
            Just d -> return d
            Nothing -> readJsonModelErrorHandler
-- readJsonModelFromDisk filePath
--     = do
--         let byteStringData = BS.readFile filePath
--         decodedMap <- fmap Aeson.decode byteStringData
--         case decodedMap of
--             Just d -> return (Map.assocs (d :: Map.Map String Int))
--             Nothing -> readJsonModelErrorHandler

-- Not sure if this is sufficient, but the aim of the function is to somewhat aid in Error handling 
-- in instances when the Aeson.decode function is not able to read a ByteString map from disk
readJsonModelErrorHandler :: IO (Map String Int)
readJsonModelErrorHandler
    = do
        putStrLn "Error in readJsonModelFromDis: Either the ByteString file contained trailing data; or\n\tAeson.decode failed due to incomplete or invalid input."
        return Map.empty

-- Assumes that the modelsPath directory exists
-- Writes a Map where `key` :: String (the word) and `value` :: Int (number of occurences in the train dataset) 
-- Encoded Map data is written as ByteString into the created filePath location
writeMapModelToDiskAsJSON :: String -> Map String Int -> IO ()
writeMapModelToDiskAsJSON fileName mapData
    = do
        -- remove "" key (for some reason it is not removed earlier in getDirContents)
        let encodedMap = Aeson.encode (Map.delete "" mapData) 
        let filePath = modelsPath ++ "/" ++ fileName ++ ".json"
        BS.writeFile filePath encodedMap