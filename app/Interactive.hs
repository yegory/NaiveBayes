module Interactive where

import System.IO
import Text.Read   (readMaybe)
import Tools

import Data.Map (Map)
import qualified Data.Map as Map
-- To run it, try:
-- ghci Interactive
-- start

{- ====================================== DATA DEF ====================================== -}

welcomeText = "\n~\tHi human!\n~\tWelcome to the Naive Bayes application\n\n"
helpPrompt = "Type a command; 'help' for a list of commands; or 'exit' to exit: "
exitMessage = "\n\t--------------------------- Bye! Bye! ---------------------------\n"
listOfCommands = ["\nList of commands:",
                  "  ‣ Enter 'state' to get info about the current model selected (if any)",
                  "  ‣ Enter 'train <val>' to start training a model\n\t<val> can be one of A, B, or C:\n\t| A := 25%-75% train-test split\n\t| B := 50%-50% train-test split\n\t| C := 75%-25% train-test split",
                  "  ‣ Enter 'models' for a list of existing <model_name>`s",
                  "  ‣ Enter 'load <model_name>' to load an existing model",
                  "  ‣ Enter 'validate' to validate the currently selected model, or 'validate <model_name>' to validate a specific model.\n     Gives an error if no model is selected or <model_name> is not in \"./models\" folder"]
                --   "  5. Enter 'help' for this list of commands.",
                --   "  6. Enter 'exit' to exit the program."]

-- need
-- array of saved models
-- 

{- ======================================== MAIN ======================================== -}

data State = Empty
            | LoadedModel ModelName ModelMap ValidationScore
        deriving (Ord, Eq, Show)

type ModelMap = Map String Int
type ModelName = Maybe String
type ValidationScore = Maybe Double 


start :: IO ()
start
    = do
        putStr welcomeText
        let emptyState = LoadedModel Nothing Map.empty Nothing
        main_loop emptyState

main_loop :: State -> IO ()
main_loop state =
   do
        line <- getResponse helpPrompt
        jsonFileNames <- getAllJsonFileNames
        let lline = lowerString $ trim line
        case lline of
            v   |   s `elem` ["exit", "e"]                                                           -> exit
                |   s `elem` ["state", "s"]                                                          -> printStateMaybe state
                |   s `elem` ["help", "h"] && n == 2 && (head t) `elem` ["train", "t"]               -> trainHelp state
                |   s `elem` ["help", "h"]                                                           -> printHelp state
                |   s `elem` ["models", "m"]                                                         -> printModels state
                |   s `elem` ["train", "t"] && n == 2 && lowerString (head t) `elem` ["a", "b", "c"] -> trainModelHandler state (lowerString (head t)) 
                |   s `elem` ["train", "t"] && n == 2 && lowerString (head t) `elem` ["help", "h"]   -> trainHelp state
                |   s `elem` ["train", "t"]                                                          -> trainError state     
                |   s `elem` ["load", "l"] && n == 2 && head t `elem` jsonFileNames                  -> loadModelHandler state (head t) 
                |   s `elem` ["load", "l"] && n == 2                                                 -> loadErrorModelNotFound state (head t) 
                |   s `elem` ["load", "l"] && n > 2                                                  -> loadErrorTooManyArguments state (n - 1)
                |   s `elem` ["load", "l"]                                                           -> loadErrorNotEnoughArguments state
                |   s `elem` ["validate", "v"] && n == 2 && head t `elem` jsonFileNames              -> validateModelHandler state (head t) 
                -- |   s `elem` ["validate", "v"]                                                    -> validate v case analysis for if the State has a model loaded up for verifying
                |   otherwise -> main_loop state
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
printStateMaybe state@(LoadedModel modelName modelMap validationScore)
    = do
        case modelName of
            Just modelName -> printState modelName modelMap validationScore
            Nothing -> putStrLn "Currently no model is selected :(\n\tHang on though! You can train a new model or select an existing one!\n"
        main_loop state

printState :: String -> Map String Int -> ValidationScore -> IO ()
printState modelName modelMap validationScore
    = do
        putStrLn ("Model's file name (without extension): " ++ modelName)
        putStrLn ("It's map contains " ++ show (Map.size modelMap) ++ " unique words (keys)")
        -- TODO: add total count of words maybe or some interesting statistic?
        case validationScore of
            Just validationScore -> putStrLn ("And the validation score is: " ++ (show validationScore))
            Nothing -> putStrLn "The model currently does not have a validation score, try the 'validate' command!"

printModels :: State -> IO ()
printModels state
    = do
        jsonFileNames <- getAllJsonFileNames
        printStringArray jsonFileNames
        main_loop state

trainModelHandler :: State -> String -> IO ()
trainModelHandler state option
    = do
        putStrLn option
        main_loop state


-- triggered when a valid model file with a base name of modelName was provided by user
loadModelHandler :: State -> String -> IO ()
loadModelHandler state modelName -- maybe state is not needed since we are 
    = do
        -- we need to load the current model into state
        -- putStrLn modelName
        readModel <- readJsonModelFromDisk modelName :: IO (Map String Int)
        if readModel == Map.empty
            then putStrLn "Warning: the loaded model is empty!"
            else putStrLn "Model read!"
        let newState = LoadedModel (Just modelName) readModel Nothing
        main_loop newState

validateModelHandler :: State -> String -> IO ()
validateModelHandler state modelName
    = do
        putStrLn modelName
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
        putStrLn "\nBadNumberOfArgumentsError: Expected 1 argument after 'train' representing the train mode. Type 'help train' for information about the 'train' subcommand\n"           
        main_loop state

loadErrorNotEnoughArguments :: State -> IO ()
loadErrorNotEnoughArguments state 
    = do
        putStrLn "\nBadNumberOfArgumentsError: Expected 1 argument after 'load', but got 0\n"     
        main_loop state

loadErrorTooManyArguments :: State -> Int -> IO ()
loadErrorTooManyArguments state nArgs 
    = do
        putStrLn ("\nTooManyArgumentsError: Expected 1 argument after 'load', but got " ++ show nArgs ++ "\n")
        main_loop state

loadErrorModelNotFound :: State -> String -> IO ()
loadErrorModelNotFound state model
    = do
        putStrLn ("\nModelNotFoundError: The model '" ++ model ++ "' was not found in the in the \"./models\" folder.\n")
        main_loop state