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


main :: IO ()
main 
    = do 
        start
        --- TRAIN THE MODEL
        -- accuracy <- trainAndValidate

        -- comment below out if you want
        -- let fileName = "example"
        -- writeMapModelToDiskAsJSON fileName map_train_pos
        -- s <- readJsonModelFromDisk fileName
        -- printMap (Map.assocs s)
        -- start

        putStr "" -- prevent errors in case no IO is performed 


-- create the vocabulary set by combining keys from positive and negative maps
-- perform laplace smoothing 
-- convert to log probabilities
-- output the model as a tuple of the pos and neg log prob dicts and the vocabulary


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