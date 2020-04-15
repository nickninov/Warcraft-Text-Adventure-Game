-- Main file to load the game!

-- Import files
import Effects
import Functions
import DataTypes
import Actions

-- Import libraries
import System.IO
import System.IO.Unsafe
import System.Directory
import System.IO.Unsafe
import System.Console.ANSI
import Text.Read
-- Launches the game
main :: IO ()
main = do

    let file = "Character.txt"

    -- Show the player their options
    options
    
    -- Get user's option
    putStr "Option: "
    option <- getChar
    putStrLn "\n"

    -- Check if selected option is valid
    if option == '1' || option == '2'
        then do
            -- Start new game
            if option == '1'
                then launchNewGame file
            -- Load game file
            else loadGameFile file
    -- Option was invalid
    else main


-- New game start
launchNewGame :: FilePath -> IO()
launchNewGame file = do
    -- Initial starting details
    let x = 1
    let y = 1
    let health = 200.00
    let crit = 0.5
    let inventory = [Item "Malte's Axe" "Forged by the Pumping Lemma of Context Free Languages" 1.0 10]
    let item = Item "Blademaster Sword" "A traditional blade from the Burning Blade clan." 4.0 30
    let weaponDmg = weaponDamage $ item
    let spells = [(("1) Heroic Strike", "A strong attack that increases melee damage. Can be casted all the time."), weaponDmg * 10), (("2) Mortal Strike", "A vicious strike that deals weapon damage. Can be casted on enemies below 70% health"), weaponDmg * 15), (("3) Bladestorm", "Become an unstoppable storm of destructive force. Can be casted on enemies below 50%"), weaponDmg * 20), (("4) Execute", "Attempt to finish off a wounded foe. Can be casted to enemy below 20%."), weaponDmg * 25) ]
    let gold = 0
    let character = Character "Samuro" spells crit health health inventory item gold

    -- Write data on the file
    let fileData = ""++show character ++ "\n" ++ show x ++ "\n" ++ show y
    writeFile file fileData

    -- Intro effect - imported from Effects.hs
    intro

    -- Start moving in the game - imported from Movement.hs
    action x y character

-- Load game start
loadGameFile :: FilePath -> IO()
loadGameFile file = do
    let exist = unsafePerformIO $ doesFileExist file
    
    -- File exists
    if exist == True
        then do
            -- Open file
            fileData <- openFile file ReadMode
            
            -- Read character data
            charStr <- hGetLine fileData            
            -- Try to convert Character type from String to Character
            let character = readMaybe charStr :: Maybe Character

            -- Read X coordinate
            xStr <- hGetLine fileData
            -- Try to convert X type from String to X
            let x = readMaybe xStr :: Maybe X

            -- Read Y coordinate
            yStr <- hGetLine fileData
            -- Try to convert Y type from String to Y
            let y = readMaybe yStr :: Maybe Y

            -- Close file
            hClose fileData
            
            -- Check if file has been damaged
            if character == Nothing || x == Nothing || y == Nothing
                then do
                    setSGR [SetColor Foreground Vivid Red]
                    slowTextRec "File has been damaged. Launching a new game.\n" 20000
                    launchNewGame file
            -- File is not damaged
            else do
                let newChar = removeCharacterMaybe character
                let newX = removeCoordinateMaybe x
                let newY = removeCoordinateMaybe y
                action newX newY newChar
    -- File does not exist
    else do 
        setSGR [SetColor Foreground Vivid Red]
        slowTextRec "File does not exist...\n" 20000
        main