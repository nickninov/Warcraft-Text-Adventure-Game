-- This file contains all the combat mechanisms of the game
module Actions where

-- Import files
import DataTypes
import Effects
import Functions

-- Import libraries
import System.Random
import System.Console.ANSI
import Data.Char

-- Combat logic

-- Check player's and enemy's health
healthStatus :: Character -> Character -> X -> Y -> IO ()
healthStatus player enemy x y
    -- Both player and enemy are alive
    | health player > 0 && health enemy > 0 = combat player enemy x y
    -- Enemy has been slain
    | health player > 0 && health enemy <= 0 = do 
        killedEnemy player enemy x y
        battleCry player x y
    -- Player has been slain
    | health player <= 0 && health enemy > 0 = killedByEnemy enemy
    | otherwise = putStr "Both player and enemy died..."

-- The combat logic
combat :: Character -> Character -> X -> Y -> IO ()
combat player enemy x y = do
    -- Display user's health
    showHealth player
    -- Display enemy's health
    showHealth enemy
    putStrLn ""
    -- Show user's spells
    showSpells $ attacks player
    -- User selects a spell
    putStr "Option: "
    playerOption <- getChar
    putStrLn "\n"

    -- Enemy generates a spell
    let enemyOption = enemySpell enemy (intToDigit $ getRandomNumber 1 3)

    -- Check if player's input is valid and returns the spell's attack
    let status = playerSpell player playerOption

    -- Check if player can attack
    if fst status == True
        then do
            
            -- Check if player should crit
            let critStatusPlayer = shouldCrit (getRandomNumber 1 10) (getRandomNumber 11 21)
            let dmgPlayer = critChance critStatusPlayer player (snd status)

            -- Display player's damage
            putStr "Damage dealt to "
            putStr $ name enemy
            putStr ": "
            print dmgPlayer
            putStrLn ""

            -- Deal damage to enemy
            let newEnemyStats = attackEnemy dmgPlayer enemy

            -- Check if enemy should crit
            let critStatusEnemy = shouldCrit (getRandomNumber 1 50) (getRandomNumber 51 100)
            let dmgEnemy = critChance critStatusEnemy enemy enemyOption

            -- Deal damage to player
            let newPlayerStats = attackPlayer enemyOption player
            putStr "Damage dealt to "
            putStr $ name player
            putStr ": "
            print $ enemyOption
            putStrLn "\n"

            -- Check characters' health
            healthStatus newPlayerStats newEnemyStats x y
    else
        healthStatus player enemy x y
    return ()

-- Shows the player the spells and their damage
showSpells :: Spells -> IO ()
showSpells [] = return ()
showSpells (x:xs) = do
    showSkills (fst $ fst x) (snd $ fst x) (show $ snd x) 20000
    putStrLn ""
    showSpells xs

-- Returns the selected spell's attack
playerSpell :: Character -> Option -> AttackStatus
playerSpell player input 
    | input == '1' || input == '2' || input == '3' = do
            let dmg = snd ([x | x <- (attacks player), any (==input) (fst $ fst x)] !! 0)
            (True, dmg)
    | otherwise = (False, 0)

-- Returns the selected spell's damage
enemySpell :: Character -> Option -> Attack
enemySpell enemy input = snd ([x | x <- (attacks enemy), any (==input) (fst $ fst x)] !! 0)

-- Attacks the enemy
attackEnemy :: Attack -> Character -> Character
attackEnemy playerAttack enemy = newEnemy 
    where
        -- Enemy stats with lower health
        newName = name enemy
        attack = attacks enemy
        critical = crit enemy
        enemyHP = health enemy - playerAttack
        maxHP = maxHealth enemy

        newEnemy = Character newName attack critical enemyHP maxHP

-- Attacks the player
attackPlayer :: Attack -> Character -> Character
attackPlayer enemyAttack player = newPlayer
    where
        -- Player stats with lower health
        newName = name player
        attack = attacks player
        critical = crit player
        playerHP = health player - enemyAttack
        maxHP = maxHealth player

        newPlayer = Character newName attack critical playerHP maxHP

-- Critical chance calculation
shouldCrit :: Int -> Int -> Bool
shouldCrit rng1 rng2 = even $ div rng2 rng1

-- Check if attack has a crit
critChance :: Bool -> Character -> Attack -> Attack
critChance status character dmg
    | status == True = ((crit character) * dmg) + dmg
    | otherwise = dmg

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Movement logic

-- Ask which way to go
movement :: X -> Y -> Character -> IO ()
movement x y player = do

    -- Get user desired location
    putStr ("Go: ")
    direction <- getLine 
    putStrLn ""

    let dir = removeMaybe $ stringToMovement $ toUp direction

    -- Check if input was valid
    if dir == Exit
        then movement x y player
    else do
        -- Move inside grid
        let newCord = move dir x y
        
        -- Display a random move quote
        randomMoveQuote (getRandomNumber 1 4) 20000

        -- Check what has happened to the player
        let action = checkNextAction (fst newCord) (snd newCord)

        checkPlayerStatus action (fst newCord) (snd newCord) player

    return ()

-- Convert the String movement to a Maybe Movement data type
stringToMovement :: String -> Maybe Movement
stringToMovement "North" = Just North
stringToMovement "South" = Just South
stringToMovement "East" = Just East
stringToMovement "West" = Just West
stringToMovement "Exit" = Just Exit
-- If player misspelt a word
stringToMovement _ = Nothing

-- Remove Maybe from Movement data type
removeMaybe :: Maybe Movement -> Movement
removeMaybe Nothing = Exit
removeMaybe (Just dir) = dir

-- Move position within grid
move :: Movement -> X -> Y -> Position
move direction x y 
    | direction == North = (x, decPos y)
    | direction == South = (x, incPos y)
    | direction == East = (incPos x, y)
    | direction == West =  (decPos x, y)

-- Check player's next action
checkNextAction :: X -> Y -> CharacterStatus
checkNextAction x y
    -- End of map - North - go back 1 block
    | (x, y) == (1, -1)
        || (x, y) == (2, -1)
        || (x, y) == (4, -1)
        || (x, y) == (5, -1)
        || (x, y) == (6, -1)
        || (x, y) == (8, -1)
        || (x, y) == (9, -1) = NorthBack 
    -- End of map - West - go back 1 block
    | (x, y) == (-1, 1)
        || (x, y) == (-1, 1)
        || (x, y) == (-1, 2)
        || (x, y) == (-1, 3) = WestBack
    -- End of Map
    | (x, y) == (5, 5)
        || (x, y) == (6, 4)
        || (x, y) == (9, 3) = SouthBack
    -- Bumped into the Fel fire wall - from the right side
    | (x, y) == (4, 0) 
        || (x, y) == (4, 2)
        || (x, y) == (4, 3) = FelFireWallRight
    -- Bumped into the Fel fire wall - from the left side
    | (x, y) == (3, 0)
        || (x, y) == (3, 2)
        || (x, y) == (3, 3) = FelFireWallLeft
    -- Bumped into the initial Fel lava - (0, 0)
    | (x, y) == (0, 0) = FelLavaStart
    -- Bumped into Fel lava
    | (x, y) == (7, 0)
        || (x, y) == (7, 1)
        || (x, y) == (7, 3) = FelLava
    -- Bumped into a Fel drink
    | (x, y) == (5, 2)
        || (x, y) == (8, 1) 
        || (x, y) == (8, 2) = FelDrink
    -- Combat mob(s)
    | (x, y) == (0, 3)
        || (x, y) == (2, 0)
        || (x, y) == (2, 1)
        || (x, y) == (2, 3) 
        || (x, y) == (4, 1)
        || (x, y) == (5, 0) 
        || (x, y) == (5, 3)
        || (x, y) == (7, 2) = Combat
    -- Bumped into Arechron 
    | (x, y) == (6, 2) = Arechron
    -- Combat boss
    | (x, y) == (3, 1)
        || (x, y) == (9, 0)
        || (x, y) == (9, 1)
        || (x, y) == (9, 2) = Boss
    -- Finish points
    | (x, y) == (10, 0)
        || (x, y) == (10, 1)
        || (x, y) == (10, 2) = Finish
    -- Teleport user to new location
    | (x, y) == (0, 4) -- Teleports to (4, 1)
        || (x, y) == (1, 4) -- Teleports to (5, 3)
        || (x, y) == (2, 4) = Portal -- Teleports to (7, 2)
    -- Bump into a dead corpse
    | (x, y) == (1, 3) 
        || (x, y) == (5, 4) = Corpse
    -- Bump into a person on this realm
    | (x, y) == (1, 2)
        || (x, y) == (5, 1) = Person
    -- Player can walk
    | otherwise = Walk

-- Check what has to happen to the player
checkPlayerStatus :: CharacterStatus -> X -> Y -> Character -> IO ()
checkPlayerStatus status x y player
    -- Check if the player is dead
    | status == Dead = do
        setSGR [SetColor Foreground Dull Yellow]
        putStrLn "Game over. You died!\n"
        setSGR []
    -- Check if the player is in combat
    | status == Combat = do
        let enemy = getEnemy (getRandomNumber 1 3)
        healthStatus player enemy x y
    | status == Boss = do
        let boss = Character "Pit Lord" [(("1) Slam", "Slams the opponent."), 10.00), (("2) Fel Fire Nova", "Emitting a steady pulse of fel fire, dealing damage to the player."), 50.00), (("3) Overpower", "Instantly overpower the enemy, causing high weapon damage."), 70.00)] 3.0 1000.00 1000.00
        healthStatus player boss x y
    -- Check if player has reached North map end 
    | status == NorthBack = do
        endOfMap
        movement x (incPos y) player
    -- Check if player has reached South map end
    | status == SouthBack = do
        endOfMap
        movement x (decPos y) player
    -- Check if player has bumped into a Fel river
    | status == FelDrink = felBlood player x y
    -- Check if player has reached Fel lava - go back (x - 1, y)
    | status == FelLava = do
        felLava
        movement (decPos x) y player
    -- Check if player has reached the initial Fel lava - (0, 0) - go to (0, 1)
    | status == FelLavaStart = do
        endOfMap
        movement x (incPos y) player
    -- Check if player has reached West map end
    | status == WestBack = do
        endOfMap
        movement (incPos x) y player
    -- Check if player is alive and can freely move
    | status == Walk = movement x y player
    -- Check if player has to fight an enemy
    | status == Combat = do
        let enemy = getEnemy (getRandomNumber 1 3)
        healthStatus player enemy x y
    -- Check where to teleport player
    | status == Portal = do 
        portal
        teleport player x y
    -- Check if player has bumped into Arechron
    | status == Arechron = do 
        dialogueArechron
        narusGift player x y

    -- Check if player has bumped into a corpse
    | status == Corpse = do
        corpse
        movement x y player
    -- Check if player is on the right side of the Fel Fire wall
    | status == FelFireWallRight = do
        felFireWallBump
        movement (incPos x) y player
    -- Check if player is on the left side of the Fel Fire wall
    | status == FelFireWallLeft = do
        felFireWallBump
        movement (decPos x) y player
    -- Check if player has bumped into a person
    | status == Person = do
        person (getRandomNumber 1 3) 20000
        movement x y player
    -- Check if player has finished the game
    | status == Finish = finish

-- Generates a mob for the player to fight
getEnemy :: Int -> Character
getEnemy n 
    | n == 1 = Character "Felguard" [(("1) Axe Toss", "The felguard hurls his axe"), 30.00), (("2) Legion Strike", "A sweeping attack that does damage"), 35.00), (("3) Overpower", "Charge the player and deal damage."), 40.00)] 1 300.00 300.00
    | n == 2 = Character "Imp" [(("1) Felbolt", "Deal fel fire damage to the player"), 10.00), (("2) Scorch", "Burn the player with fel fire"), 15.00), (("3) Rain of Fel", "Fire a fel meteorite shower"), 30.00)] 0.2 150.00 150.00
    | n == 3 = Character "Voidlord" [(("1) Void grab", "Deal void damage to the player "), 30.00), (("2) Void bolt", "Send a void bolt to the player "), 35.00), (("3) Drain soul", "Drain the player's soul"), 50.00)] 0.8 200.00 200.00

-- Teleport player
teleport :: Character -> X -> Y -> IO ()
teleport player x y
    | (x, y) == (0, 4) = do 
        let enemy = getEnemy (getRandomNumber 1 3)
        healthStatus player enemy 4 1
    | (x, y) == (1, 4) = do
        let enemy = getEnemy (getRandomNumber 1 3)
        healthStatus player enemy 5 3 
    | (x, y) == (2, 4) = do
        let enemy = getEnemy (getRandomNumber 1 3)
        healthStatus player enemy 7 2



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Buffs logic

-- Increasess player's damage by 0.5%
battleCry :: Character -> X -> Y -> IO ()
battleCry player x y = do
    let newName = name player
    let newHealth = maxHealth player
    let newMaxHealth = maxHealth player
    let newCrit = crit player
    let newAttacks = map (\x -> ((fst x), ((snd x) + ((snd x) * 0.05)))) (attacks player)
    let newChar = Character newName newAttacks newCrit newHealth newMaxHealth

    dialogue "Samuro" "For the Burning Blade!\n" 20000
    dialogue "Battle cry" "Increase overall damage by 0.5% for all of your spells!\n" 20000
    slowTextRec "Your wounds magically heal faster. You have been restored to maximum health.\n" 20000

    -- Move character
    movement x y newChar

-- Explains to the user what Fel Blood does.
felBlood :: Character -> X -> Y -> IO ()
felBlood player x y = do
    felOffer
    -- Get the user's input
    putStr "Choice: "
    choice <- getLine
    putStrLn ""
    -- Check input
    felChoice player x y (map toLower choice)

-- Increases the player's damage but decreases their maximum health if they have selected yes
felChoice :: Character -> X -> Y -> String -> IO ()
felChoice player x y choice
    | choice == "yes" = do
        let newName = name player
        -- Increase the critical chance by 50%
        let newCrit = crit player + (crit player * 0.5)
        -- Increase the damage by 50%
        let newAttacks = map (\x -> ((fst x), (snd x + (snd x * 0.5)))) (attacks player)
        -- Decrease maximum health
        let newMaxHealth = maxHealth player - (maxHealth player * 0.3)
        -- Decrease current health
        let newHealth = checkHealth (maxHealth player) (health player) newMaxHealth
        
        let newChar = Character newName newAttacks newCrit newHealth newMaxHealth
        setSGR [SetColor Foreground Vivid Green]
        slowTextRec "You have accepted Gul'dan's offer. Now you feel stronger!\n" 20000

        movement x y newChar
    | choice == "no" = do
        slowTextRec "You have rejected Gul'dan's gift.\n" 20000
        movement x y player
    | otherwise = felBlood player x y

-- Check where the player's health is
checkHealth :: Health -> Health -> Health -> Health
checkHealth old current new
    -- Check if player's current health is between the old max health and new max health
    | old >= current && current >= new = current - (current - new)
    -- check if player's current health is lesser or equal to the new health
    | new >= current = current

narusGift :: Character -> X -> Y -> IO ()
narusGift player x y = do

    let newName = name player
    let newCrit = crit player 
    let newAttacks = attacks player
    -- Increase maximum health 3 times
    let newMaxHealth = maxHealth player * 3
    -- Heal player to maximum health
    let newHealth = newMaxHealth
    
    let newChar = Character newName newAttacks newCrit newHealth newMaxHealth
    -- Naru's gift info
    dialogue "Naru's gift" "Heals you and tripples your health.\n" 20000
    movement x y newChar