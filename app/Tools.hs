module Tools where

import Data.Char (ord, chr)
import System.FilePath (takeExtension, takeBaseName)
import Data.List (sort)
-- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html#v%3agetDirectoryContents
import System.Directory (doesFileExist, getDirectoryContents)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{- ====================================== DATA DEF ====================================== -}

punctuation = "!\"#$%&'()*+, -./:;<=>?@[]^_`{|}~ "

datasetsPath = "../datasets" :: FilePath
modelsPath = "../models" :: FilePath
validationModelsPath = "../models/validationFilePathData"
computedModelScoresPath = "../models/validatedModelMap/scores.json"

trainPositivePath   = datasetsPath ++ "/train_positive" :: FilePath
trainNegativePath   = datasetsPath ++ "/train_negative" :: FilePath
testPositivePath    = datasetsPath ++ "/test_positive"  :: FilePath
testNegativePath    = datasetsPath ++ "/test_negative"  :: FilePath
stopwordsFilePath   = datasetsPath ++ "/stopwords.txt"  :: FilePath
exampleDirPath      = datasetsPath ++ "/exampleDir"

type Model = (Map String Double, Map String Double, Set String)

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



-- Assumes models path exists, returns the names of every model in modelsPath folder
getAllModelNames :: IO [String]
getAllModelNames 
    = do
        dirContents <- getDirectoryContents modelsPath
        return $ sort (map takeBaseName (filter isJsonFile dirContents))

readJsonModelFromDisk :: FilePath -> IO (Maybe (Map String Int))
readJsonModelFromDisk fileName
    = do
        let filePath = modelsPath ++ "/" ++ fileName ++ ".json"
        let byteStringData = BS.readFile filePath
        decodedMap <- fmap Aeson.decode byteStringData
        case decodedMap of
            Just model -> return model
            Nothing -> return Nothing


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


-- convert directory contents defined by path list, create a count dictionary of tokens
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


writeTrainedModelToDiskAsJSON :: String -> (Map String Double, Map String Double, Set String) -> IO ()
writeTrainedModelToDiskAsJSON fileName mapData
    = do
        -- remove "" key (for some reason it is not removed earlier in getDirContents)
        let encodedMap = Aeson.encode mapData
        let filePath = modelsPath ++ "/" ++ fileName ++ ".json"
        BS.writeFile filePath encodedMap
    

readTrainedModelFromDisk :: FilePath -> IO (Maybe (Map String Double, Map String Double, Set String))
readTrainedModelFromDisk fileName
    = do
        let filePath = modelsPath ++ "/" ++ fileName ++ ".json"
        let byteStringData = BS.readFile filePath
        decodedMap <- fmap Aeson.decode byteStringData
        case decodedMap of
            Just model -> return model
            Nothing -> return Nothing


getStopWords :: IO [String]
getStopWords
    = do
        s <- readFileStrict stopwordsFilePath :: IO String
        let stopWords = splitOn ['\n'] s :: [String]
        return stopWords




--- general procedure to train and validate the model given our dataset
trainAndValidate :: IO (Double)
trainAndValidate = do     
    -- create arraypath of each file.txt in directory
    trainPositiveFilePaths <- getAllTxtFilePaths trainPositivePath :: IO [String]
    trainNegativeFilePaths <-getAllTxtFilePaths trainNegativePath :: IO [String]
    testPositiveFilePaths <- getAllTxtFilePaths testPositivePath :: IO [String] 
    testNegativeFilePaths <-getAllTxtFilePaths testNegativePath :: IO [String]
    -- get the array of stop words to remove
    stopWords <- getStopWords
    map_train_pos <- getDirContents Map.empty stopWords trainPositiveFilePaths :: IO (Map String Int)
    map_train_neg <- getDirContents Map.empty stopWords trainNegativeFilePaths :: IO (Map String Int)
    model <- trainModel map_train_pos map_train_neg
    -- putStr (show model)

    -- result <- inference model "it is a bad movie, the director was terrible." stopWords
    -- putStr (show result)

    --- COMPUTE THE accuracy
    accuracy <- validationAccuracy model testPositiveFilePaths testNegativeFilePaths stopWords
    putStr (show accuracy)
    return accuracy


---  THE FOLLOWING FUNCTIONS ARE FOR VALIDATING THE MODEL WITH TEST EXAMPLES
--- calculates validation accuracy given paths to negative and positive examples
--- alongside with the model to be validated
validationAccuracy :: Model -> [String] -> [String] -> [String] -> IO (Double)
validationAccuracy model posFilePaths negFilePaths stopWords= do
    valPos <- validateMultiple model posFilePaths stopWords
    valNeg <- validateMultiple model negFilePaths stopWords
    let correctPos = sum (map (\bool -> if bool then 1.0 else 0.0) valPos)
    let correctNeg = sum (map (\bool -> if bool then 0.0 else 1.0) valNeg)
    let incorrectPos = fromIntegral(length valPos) - correctPos
    let incorrectNeg = fromIntegral(length valNeg) - correctNeg
    let accuracy = (correctPos + correctNeg) / (correctPos + correctNeg + incorrectPos + incorrectNeg)
    return accuracy

--- validates multiple examples given their paths 
validateMultiple :: Model -> [String] -> [String] -> IO([Bool])
validateMultiple model paths stopwords = mapM (\path -> validateSingle model path stopwords) paths
--- validates a simple example given a path and outputs IO True if the example is positive
validateSingle :: Model -> String -> [String] ->  IO (Bool)
validateSingle model path stopwords = do
    review <- readFileStrict path
    inference model review stopwords



{- ====================================== CODE FOR TRAINING THE MODEL ====================================== -}

-- produce a total vocabulary of tokens given dictionaries of positive and negative examples
makeVocabulary :: Map String Int -> Map String Int -> Set String
makeVocabulary dictPos dictNeg =  Set.fromList (map (\(x,y) -> x) (Map.toList (Map.union dictNeg dictPos)))

--- performs laplace smoothing in a count dictionary against the vocabulary
laplaceSmoothing :: Map String Int -> Set String -> Map String Int
laplaceSmoothing dict vocab = 
    let difference = Map.difference (Map.fromList (map (\token -> (token, 1)) (Set.toList vocab))) dict -- create Difference between vocab and dict
    in
        Map.union difference (Map.map (\count -> count + 1) dict)

--- converts a count dicitonary into a log probability dictionary
getLogProbs :: Map String Int -> Map String Double
getLogProbs dict = let total = foldr (+) 0 dict
        in (Map.map (\x -> log (fromIntegral x/fromIntegral total)) dict)
            

-- trains the Naive Bayes model given count dicitonaries of positive and negative examples
trainModel :: Map String Int -> Map String Int -> IO Model
trainModel dictPos dictNeg = do
    let vocabulary = makeVocabulary dictPos dictNeg
    let smoothedPosCounts = laplaceSmoothing dictPos vocabulary
    let smoothedNegCounts = laplaceSmoothing dictNeg vocabulary
    let logProbsPos = getLogProbs smoothedPosCounts
    let logProbsNeg = getLogProbs smoothedNegCounts
    return (logProbsPos, logProbsNeg, vocabulary)

--- CODE FOR CONDUCTING MODEL INFERENCE

-- given a string representing a movie review and stopwords, produce True if review is positive
inference :: Model -> String -> [String] -> IO Bool
inference (dictPos, dictNeg, vocab) review stopWords = do
    -- we want to parse the string into tokens
    let xSplitLower = map (\s -> lowerString (filter keepletter s)) (splitOn punctuation review) :: [String]
    let tokenList = filter (`notElem` stopWords) xSplitLower :: [String]
    -- calculate log probability for being in positive
    let logProbPos = sumLogProbs tokenList dictPos
    let logProbNeg = sumLogProbs tokenList dictNeg
    return (logProbPos > logProbNeg) -- NOTE the direction is reversed here ... there could be a bug

--- given a list of tokens and log probability dicitonary of tokens,
--- calculate a sum of log probabilities of the corresponding tokens in the list
sumLogProbs :: [String] -> Map String Double -> Double
sumLogProbs tokenList dict = sum (map (\token -> 
                            let value = Map.lookup token dict in
                                Maybe.fromMaybe 0 value) tokenList)






writeValidationFilePathArraysToDiskAsJSON :: String -> ([FilePath], [FilePath]) -> IO ()
writeValidationFilePathArraysToDiskAsJSON fileName paths@(positiveTestPaths, negativeTestPaths)
    = do
        -- remove "" key (for some reason it is not removed earlier in getDirContents)
        let pathsEncoded = Aeson.encode paths
        let filePath = validationModelsPath ++ "/" ++ fileName ++ ".json"
        BS.writeFile filePath pathsEncoded

readJsonValidationFilePathArrays :: String -> IO (Maybe ([FilePath], [FilePath]))
readJsonValidationFilePathArrays fileName 
    = do
        let filePath = validationModelsPath ++ "/" ++ fileName ++ ".json"
        let byteStringData = BS.readFile filePath
        decodedPaths <- fmap Aeson.decode byteStringData
        case decodedPaths of
            Just paths -> return paths
            Nothing -> return Nothing

-- if the model with the key: `modelName` exists in the `computedModelScoresPath` file, then return it
-- otherwise return Nothing
getModelEvaluationIfExists :: String -> IO (Maybe Double)
getModelEvaluationIfExists modelName
    = do
        -- check if the scores.json has been created yet
        exists <- doesFileExist computedModelScoresPath :: IO Bool
        case exists of
            True -> do
                    let byteStringData = BS.readFile computedModelScoresPath
                    decodedScoreMap <- fmap Aeson.decode byteStringData
                    case decodedScoreMap of
                        Just map -> do
                                    case (Map.lookup modelName map) of
                                        Just score -> return score
                                        Nothing -> return Nothing 
                        Nothing -> return Nothing
            False -> return Nothing
        
-- add the model score to the `computedModelScoresPath` file
writeModelEvaluation :: String -> Double -> IO ()
writeModelEvaluation modelName evalScore
    = do
        -- check if the scores.json has been created yet
        exists <- doesFileExist computedModelScoresPath :: IO Bool
        case exists of
            True -> do
                    let byteStringData = BS.readFile computedModelScoresPath
                    decodedScoreMap <- fmap Aeson.decode byteStringData
                    case decodedScoreMap of
                        Just map -> do
                                    let new_map = Map.insert modelName evalScore map
                                    let new_mapEncoded = Aeson.encode new_map
                                    BS.writeFile computedModelScoresPath new_mapEncoded
                        Nothing -> putStrLn "Warning: Aeson.decode didn't work in Tools.writeModelEvaluation."
            False -> do
                    let map = (Map.singleton modelName evalScore) :: Map String Double
                    let mapEncoded = Aeson.encode map
                    BS.writeFile computedModelScoresPath mapEncoded