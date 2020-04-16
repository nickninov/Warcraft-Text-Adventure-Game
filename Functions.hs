-- This file contains more generic functions
module Functions where

-- Import files
import DataTypes
import Effects

-- Import libraries
import System.IO
import Data.Char
import System.Random
import System.IO.Unsafe
import System.Directory
import Prelude hiding (catch)
import Control.Exception
import System.IO.Error hiding (catch)

-- Cleans the screen as the :!clear command
clear :: IO ()
clear = do
    putStr "\ESC#8"
    putStr "\ESC[2J"

-- Increment by one
incPos :: Int -> Int
incPos pos = pos + 1

-- Decrement by one
decPos :: Int -> Int
decPos pos = pos - 1

-- Make the first letter uppercase in a string
toUp :: String -> String
toUp (x:xs) = (toUpper x) : xs

-- Generate a random number given the start and end value - Remove IO type
getRandomNumber :: Int -> Int -> Int
getRandomNumber start end = unsafePerformIO $ getStdRandom $ randomR (start, end)

-- Convert char to number
charToNum :: Char -> Int
charToNum '0' = 0
charToNum '1' = 1
charToNum '2' = 2
charToNum '3' = 3
charToNum '4' = 4
charToNum '5' = 5
charToNum '6' = 6
charToNum '7' = 7
charToNum '8' = 8
charToNum '9' = 9
charToNum _ = -1

-- Save Character and coordinates
saveGame :: Character -> X -> Y -> IO ()
saveGame player x y = do
    let fileData = ""++show player ++ "\n" ++ show x ++ "\n" ++ show y
    writeFile "Character.txt" fileData
    slowTextRec "Data successfully saved!\n" 20000

-- Removes a file if it exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

-- Remove Maybe from Character data type
removeCharacterMaybe :: Maybe Character -> Character
removeCharacterMaybe (Just char) = char

-- Remove Maybe from X and Y coordinates data type
removeCoordinateMaybe :: Maybe Int -> Int
removeCoordinateMaybe (Just coordinate) = coordinate

-- Get all valid X and Y positions
positions :: Positions
positions = [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),(2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(4,0),(4,1),(4,2),(4,3),(5,0),(5,1),(5,2),(5,3),(5,4),(6,0),(6,1),(6,2),(6,3),(7,0),(7,1),(7,2),(7,3),(8,0),(8,1),(8,2),(9,0),(9,1),(9,2)]

-- Check if X Y coordinates are valid
checkXandY :: X -> Y -> Positions -> Bool
checkXandY x y positions = any (== (x, y)) positions