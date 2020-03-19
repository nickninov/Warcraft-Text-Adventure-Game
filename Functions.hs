-- This file contains more generic functions
module Functions where

-- Import files
import DataTypes

-- Import libraries
import System.IO
import Data.Char
import System.Random
import System.IO.Unsafe

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
    