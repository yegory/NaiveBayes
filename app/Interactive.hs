module Interactive where

import RandomShuffle
import System.IO
import Text.Read   (readMaybe)
import Tools
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
-- To run it, try:
-- ghci Interactive
-- start

{- ====================================== DATA DEF ====================================== -}

welcomeText = "\n~\tHi human!\n~\tWelcome to the Naive Bayes application\n\n"
helpPrompt = "Type a command; 'help' for a list of commands; or 'exit' to exit: "
exitMessage = "\n\t--------------------------- Bye! Bye! ---------------------------\n"
listOfCommands = ["List of commands:",
                  "‣ Enter 'state' to get info about the current model selected (if any)",
                  "‣ Enter 'train <val> <model_name>' to train a model with the given name\n  | <val> can be one of:\n  |  A := 25%-75% train-test split\n  |  B := 50%-50% train-test split\n  |  C := 75%-25% train-test split",
                  "‣ Enter 'models' for a list of existing <model_name>`s",
                  "‣ Enter 'load <model_name>' to load an existing model",
                  "‣ Enter 'validate' to validate the currently selected model"]

-- need
-- array of saved models
-- 

{- ======================================== MAIN ======================================== -}

data State = Empty
            |  Model (Maybe LoadedModel) ValidationScore
        deriving (Ord, Eq, Show)

type LoadedModel = (ModelName, ModelMap)
type ModelMap = (Map String Double, Map String Double, Set String)
type ModelName = String
type ValidationScore = Maybe Double



start :: IO ()
start
    = do
        putStr welcomeText
        let emptyState = Model Nothing Nothing
        main_loop emptyState


main_loop :: State -> IO ()
main_loop state =
   do
        line <- getResponse helpPrompt
        jsonFileNames <- getAllModelNames
        let lline = lowerString $ trim line
        case lline of
            v   |   s `elem` ["exit", "e"]                                                           -> exit
                |   s `elem` ["state", "s"]                                                          -> printStateMaybe state
                |   s `elem` ["help", "h"] && n == 2 && (head t) `elem` ["train", "t"]               -> trainHelp state
                |   s `elem` ["help", "h"]                                                           -> printHelp state
                |   s `elem` ["models", "m"]                                                         -> printModels state
                |   s `elem` ["train", "t"] && n == 3 && lowerString (head t) `elem` ["a", "b", "c"] -> trainModelHandler state (lowerString (head t)) (head (tail t))
                |   s `elem` ["train", "t"] && lowerString (head t) `elem` ["help", "h"]             -> trainHelp state
                |   s `elem` ["train", "t"]                                                          -> trainError state     
                |   s `elem` ["load", "l"] && n == 2 && head t `elem` jsonFileNames                  -> loadModelHandler state (head t) 
                |   s `elem` ["load", "l"] && n == 2                                                 -> loadErrorModelNotFound state (head t) 
                |   s `elem` ["load", "l"] && n > 2                                                  -> loadErrorTooManyArguments state (n - 1)
                |   s `elem` ["load", "l"]                                                           -> loadErrorNotEnoughArguments state
                |   s `elem` ["validate", "v"]                                                       -> validateCurrentModelHandler state
                |   otherwise -> do
                                putStrLn ("\n~  Sorry, but the command `" ++ lline ++ "` was not found! \n~  Type help for list of commands")
                                main_loop state
                where   (s, t) = (head split, tail split) :: (String, [String])
                        split = (splitOn " " v) :: [String]
                        n = length split -- number of total arguments including the command s

{- ======================================= HELPERS ======================================= -}

exit :: IO ()
exit
    = do
        putStr exitMessage


getResponse :: String -> IO String
getResponse question
    = do
        putStr question
        getLine

printHelp :: State -> IO ()
printHelp state
    = do
        putStr "\n"
        printStringArray listOfCommands
        putStr "\n"
        main_loop state



printStateMaybe :: State -> IO ()
printStateMaybe state@(Model maybeModel validationScore)
    = do
        case maybeModel of
            Just (modelName, modelMap) -> printState modelName modelMap validationScore
            Nothing -> putStrLn "\n~  Currently no model is selected :(\n~  Hang on though! You can train a new model or select an existing one!\n"
        main_loop state


printState :: String -> (Map String Double, Map String Double, Set String) -> ValidationScore -> IO ()
printState modelName modelMap@(m1, m2, set) validationScore
    = do
        putStrLn ("Model's file name (without extension): " ++ modelName)
        putStrLn ("Trained positive map contains: " ++ show (Map.size m1) ++ " unique words (keys)")
        putStrLn ("Trained negative map contains: " ++ show (Map.size m2) ++ " unique words (keys)")
        putStrLn ("Number of unique words in both maps: " ++ show (length set) ++ " unique words (keys)")

        -- TODO: add total count of words maybe or some interesting statistic?
        case validationScore of
            Just validationScore -> putStrLn ("And the validation score is: " ++ (show validationScore))
            Nothing -> putStrLn "The model currently does not have a validation score, try the 'validate' command!"



printModels :: State -> IO ()
printModels state
    = do
        jsonFileNames <- getAllModelNames
        printStringArray jsonFileNames
        main_loop state



-- A := 25%-75% train-test split\n B := 50%-50% train-test split\n  |  C := 75%-25% train-test split
trainChoiceToRatio :: String -> IO Double
trainChoiceToRatio s
    = do
        case s of 
            v   | v == "a" -> return 0.25
                | v == "b" -> return 0.50
                | v == "c" -> return 0.75
                | otherwise -> do
                                putStrLn "Error in trainChoiceToRatio (choice not of a, b, or c)"
                                return 0.5


trainModelHandler :: State -> String -> String -> IO ()
trainModelHandler state@(Model maybeModel validationScore) option modelName
    = do
        ratio <- trainChoiceToRatio option
        trainP_FilePaths <- getAllTxtFilePaths trainPositivePath :: IO [String]
        trainN_FilePaths <-getAllTxtFilePaths trainNegativePath :: IO [String]
        testP_FilePaths <- getAllTxtFilePaths testPositivePath :: IO [String] 
        testN_FilePaths <-getAllTxtFilePaths testNegativePath :: IO [String]

        let positivePaths = trainP_FilePaths ++ testP_FilePaths
        let negativePaths = trainN_FilePaths ++ testN_FilePaths

        positiveShuffled <- shuffle positivePaths -- all positive paths that are shuffled
        negativeShuffled <- shuffle negativePaths
        
        let len_positivePaths = length positivePaths
        let len_negativepPaths = length negativePaths

        let numOfPosFilesToTake = floor (fromIntegral len_positivePaths * ratio)
        let numOfNegFilesToTake = floor (fromIntegral len_negativepPaths * ratio)
    
        let pos_ShuffledTrain = take numOfPosFilesToTake positiveShuffled
        let neg_ShuffledTrain = take numOfNegFilesToTake negativeShuffled
        let pos_ShuffledTest = drop numOfPosFilesToTake positiveShuffled
        let neg_ShuffledTest = drop numOfNegFilesToTake negativeShuffled

        s <- readFileStrict stopwordsFilePath :: IO String
        let stopWords = splitOn ['\n'] s :: [String]

        map_train_pos <- getDirContents Map.empty stopWords pos_ShuffledTrain :: IO (Map String Int)
        map_train_neg <- getDirContents Map.empty stopWords neg_ShuffledTrain :: IO (Map String Int)

        let map_tp = Map.delete "" map_train_pos 
        let map_tn = Map.delete "" map_train_neg

        model <- trainModel map_tp map_tn :: IO (Map String Double, Map String Double, Set String)

        writeTrainedModelToDiskAsJSON modelName model
        writeValidationFilePathArraysToDiskAsJSON modelName (pos_ShuffledTest, neg_ShuffledTest)
        -- putStrLn (show totalLength)
        -- printStringArray shuffledPathIndices
        
        case maybeModel of
            Just (modelName, modelMap) -> main_loop state
            Nothing -> main_loop (Model (Just (modelName, model)) Nothing)
      


-- triggered when a valid model file with a base name of modelName was provided by user
loadModelHandler :: State -> String -> IO ()
loadModelHandler state modelName 
    = do
        -- we need to load the current model into state
        -- putStrLn modelName
        readModel <- readTrainedModelFromDisk modelName :: IO (Maybe (Map String Double, Map String Double, Set String))
        case readModel of
            Nothing -> do
                        putStrLn "Warning: the loaded model is empty!"
                        main_loop state
            Just model -> do   
                                putStrLn "Model read!"
                                saved_score <- getModelEvaluationIfExists modelName :: IO (Maybe Double)     
                                case saved_score of
                                    Nothing -> main_loop (Model (Just (modelName, model)) Nothing)
                                    Just score -> main_loop (Model (Just (modelName, model)) (Just score))



validateCurrentModelHandler :: State -> IO () 
validateCurrentModelHandler state@(Model maybeModel validationScore)
    = do
        case validationScore of
            -- if validationScore exists already, it means that this model was evaluated so just output it
            Just score -> putStrLn ("The model evaluated test files with " ++ show (score) ++ " accuracy!")
            Nothing -> 
                do
                    case maybeModel of
                        -- if the model exists at least, then
                        Just a@(modelName, model) -> do -- validate current model
                                                saved_score <- getModelEvaluationIfExists modelName :: IO (Maybe Double)
                                                case saved_score of
                                                    Just value -> putStrLn ("The model evaluated test files with " ++ show (value) ++ " accuracy!")
                                                    Nothing -> do
                                                            validationPaths <- readJsonValidationFilePathArrays modelName :: IO (Maybe ([FilePath], [FilePath]))
                                                            case validationPaths of
                                                                Just (pos_paths, neg_paths) -> do
                                                                    stopWords <- getStopWords
                                                                    score <- validationAccuracy model pos_paths neg_paths stopWords :: IO Double
                                                                    writeModelEvaluation modelName score
                                                                    putStrLn ("The model evaluated on the test files with " ++ show (score) ++ " accuracy!")
                                                                    let new_state = Model (Just a) (Just score)
                                                                    main_loop new_state
                                                                Nothing -> do
                                                                        putStrLn "Model training failed previously as couldn't find validation paths"
                                                                        main_loop state
                        -- if the model doesn't exist
                        Nothing -> do
                                putStrLn "\tLoad or train a model before performing validation!\n"
                                main_loop state

trainHelp :: State -> IO ()
trainHelp state
    = do
        putStrLn "Enter 'train <val>' to start training a model\n\t<val> can be one of A, B, or C:\n\t| A := 25%-75% train-test split\n\t| B := 50%-50% train-test split\n\t| C := 75%-25% train-test split\n"
        main_loop state

loadHelp :: State -> IO ()
loadHelp state
    = do
        putStrLn "Enter 'load <model_name>' to load an existing model\nEnter 'models' for a list of existing <model_name>`s\n"
        main_loop state

validateHelp :: State -> IO ()
validateHelp state
    = do
        putStrLn "Enter 'validate' to validate the currently selected model, or 'validate <model_name>' to validate a specific model.\n     Gives an error if no model is selected or <model_name> is not in \"./models\" foldern\n"
        main_loop state

trainError :: State -> IO ()
trainError state 
    = do
        putStrLn "\n~  BadNumberOfArgumentsError: Expected 1 argument after 'train' representing the train mode. Type 'help train' for information about the 'train' subcommand\n"           
        main_loop state

loadErrorNotEnoughArguments :: State -> IO ()
loadErrorNotEnoughArguments state 
    = do
        putStrLn "\n~  BadNumberOfArgumentsError: Expected 1 argument after 'load', but got 0\n"     
        main_loop state

loadErrorTooManyArguments :: State -> Int -> IO ()
loadErrorTooManyArguments state nArgs 
    = do
        putStrLn ("\n~  TooManyArgumentsError: Expected 1 argument after 'load', but got " ++ show nArgs ++ "\n")
        main_loop state

loadErrorModelNotFound :: State -> String -> IO ()
loadErrorModelNotFound state model
    = do
        putStrLn ("\n~  ModelNotFoundError: The model '" ++ model ++ "' was not found in the in the \"./models\" folder.\n")
        main_loop state