{- ======================================= IMPORTS ======================================= -}
import Interactive
import Tools
import System.IO

-- https://hackage.haskell.org/package/filepath-1.4.100.1/docs/System-FilePath-Posix.html
import System.FilePath 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (Maybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

punctuation = "!\"#$%&'()*+, -./:;<=>?@[]^_`{|}~ "

-- backlog of possible TODOS: ( who has time for this LOL? )
-- PERFORM VALIDATION
-- FILE IS NOT EMPTY, what should happen?
-- FILE EXISTS, read the test,

{- ======================================== MAIN ======================================== -}

type Model = (Map String Double, Map String Double, Set String)

main :: IO ()
main 
    = do 
        --- TRAIN THE MODEL
        accuracy <- trainAndValidate

        -- comment below out if you want
        -- let fileName = "example"
        -- writeMapModelToDiskAsJSON fileName map_train_pos
        -- s <- readJsonModelFromDisk fileName
        -- printMap (Map.assocs s)
        -- start

        putStr "" -- prevent errors in case no IO is performed 

--- general procedure to train and validate the model given our dataset
trainAndValidate :: IO (Double)
trainAndValidate = do     
    -- create arraypath of each file.txt in directory
    trainPositiveFilePaths <- getAllTxtFilePaths trainPositivePath :: IO [String]
    trainNegativeFilePaths <-getAllTxtFilePaths trainNegativePath :: IO [String]
    testPositiveFilePaths <- getAllTxtFilePaths testPositivePath :: IO [String] 
    testNegativeFilePaths <-getAllTxtFilePaths testNegativePath :: IO [String]
    -- get the array of stop words to remove
    s <- readFileStrict stopwordsFilePath :: IO String
    let stopWords = splitOn ['\n'] s :: [String]
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

--- CODE FOR TRAINING THE MODEL
--- produce a total vocabulary of tokens given dictionaries of positive and negative examples
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

--- trains the Naive Bayes model given count dicitonaries of positive and negative examples
trainModel :: Map String Int -> Map String Int -> IO (Model)
trainModel dictPos dictNeg = do
    let vocabulary = makeVocabulary dictPos dictNeg
    let smoothedPosCounts = laplaceSmoothing dictPos vocabulary
    let smoothedNegCounts = laplaceSmoothing dictNeg vocabulary
    let logProbsPos = getLogProbs smoothedPosCounts
    let logProbsNeg = getLogProbs smoothedNegCounts
    return (logProbsPos, logProbsNeg, vocabulary)

--- CODE FOR CONDUCTING MODEL INFERENCE

-- given a string representing a movie review and stopwords, produce True if review is positive
inference :: Model -> String -> [String] -> IO (Bool)
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