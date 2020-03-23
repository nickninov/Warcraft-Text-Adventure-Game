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
    | health player > 0 && health enemy > 0 = do
        combat player enemy x y
    -- Enemy has been slain
    | health player > 0 && health enemy <= 0 = do 
        killedEnemy player enemy x y
        -- Loot enemy
        let text = "You see something shining from " ++ (name enemy) ++ ".\n"
        slowTextRec text 20000

        lootEnemy player enemy x y
        
    -- Player has been slain
    | health player <= 0 && health enemy > 0 = killedByEnemy enemy
    -- Both player and enemy died
    | otherwise = do
         putStr "Both player and enemy died...\n"
         credits 20000

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
    let status = playerSpell player enemy playerOption

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
            print $ round dmgPlayer
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
            print $ round $ enemyOption
            putStrLn ""

            -- Check characters' health
            healthStatus newPlayerStats newEnemyStats x y
    
    -- Unsuccessful attack - enemy attacks
    else do
        punishmentText enemy
        -- Check if enemy should crit
        let critStatusEnemy = shouldCrit (getRandomNumber 1 50) (getRandomNumber 51 100)
        let dmgEnemy = critChance critStatusEnemy enemy enemyOption

        -- Deal damage to player
        setSGR [SetColor Foreground Vivid Red]
        let newPlayerStats = attackPlayer enemyOption player
        putStr "Damage dealt to "
        putStr $ name player
        putStr ": "
        print $ round $ enemyOption
        putStrLn "\n"
        setSGR []
        -- Check characters' health
        healthStatus newPlayerStats enemy x y
    return ()

-- Shows the player the spells and their damage
showSpells :: Spells -> IO ()
showSpells [] = return ()
showSpells (x:xs) = do
    showSkills (fst $ fst x) (snd $ fst x) (show $ round $ snd x) 20000
    putStrLn ""
    showSpells xs

-- Returns the selected spell's attack
playerSpell :: Character -> Character -> Option -> AttackStatus
playerSpell player enemy input 
    | input == '1'
        || input == '2' && health enemy <= maxHealth enemy * 0.7
        || input == '3' && health enemy <= maxHealth enemy * 0.5 
        || input == '4' && health enemy <= maxHealth enemy * 0.2 = do
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
        newInventory = inventory enemy
        newWeapon = weapon enemy
        newGold = gold enemy

        newEnemy = Character newName attack critical enemyHP maxHP newInventory newWeapon newGold

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
        newInventory = inventory player
        newWeapon = weapon player
        newGold = gold player

        newPlayer = Character newName attack critical playerHP maxHP newInventory newWeapon newGold

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

-- Display inventory and options
inventoryOption :: Character -> X -> Y -> IO()
inventoryOption player x y = do
    setSGR [SetColor Foreground Dull Yellow]
    let text = name player ++ "'s Gold: " ++ (show $ gold player) ++ "\n"
    -- Display gold
    slowTextRec text 20000
    setSGR []
    -- Display inventory
    slowTextRec "Inventory:" 20000
    putStrLn ""
    showInventory (inventory player) 0

    dialogue "Exit" "Press a non numeric character.\n" 20000
    putStr "Enter item index: "
    index <- getChar
    putStrLn "\n"

    let numOption = charToNum index

    -- Check if given output is valid
    if numOption /= -1
        then do
            -- Get selected item
            let item = selectItemInInventory (inventory player) numOption 0

            -- Check if item is returned
            if item /= Item "N/A" "N/A" 0.0 0
                then do
                    -- Ask to equip
                    let itemName = weaponName item
                    let text = "Would you like to equip " ++ itemName ++ "?\n"
                    slowTextRec text 20000
                
                    -- User selects an option
                    setSGR [SetColor Foreground Dull Green]
                    slowTextRec "Press the Y key for yes.\n" 20000
                    setSGR []
                    setSGR [SetColor Foreground Dull Red]
                    slowTextRec "Press the N key for no.\n" 20000
                    setSGR []

                    putStr "Option: "
                    option <- getChar
                    putStrLn "\n"

                    -- Check if option is valid
                    if option == 'y' || option == 'n'
                        then do
                            if option == 'y'
                            -- Equip item
                                then do
                                    putStrLn "Item equiped!\n"
                                    let currentWeapon = weapon player

                                    -- Change player's spells' attack
                                    let newChar = changePlayerWeapon player item numOption
                                    
                                    action x y newChar
                            else do
                            -- Do not equip item
                                putStrLn "Item not equiped!\n"
                                action x y player
                    -- The given option was invalid
                    else do
                        setSGR [SetColor Foreground Vivid Red]
                        slowTextRec "Invalid option.\n" 20000
                        setSGR []
                        slowTextRec "You close your bag and continue your adventure.\n" 20000

                        action x y player
                -- Item is not found within list
            else do
                setSGR [SetColor Foreground Vivid Red]
                slowTextRec "Item not found within your inventory!\n" 20000
                setSGR []
                slowTextRec "You close your bag and continue your adventure.\n" 20000

                action x y player
    else do
        slowTextRec "You close your bag and continue your adventure.\n" 20000
        action x y player
    
    return ()

-- Returns the selected item
selectItemInInventory :: Bag -> Int -> Int -> Item
selectItemInInventory [] _ _ = Item "N/A" "N/A" 0.0 0
selectItemInInventory (x:xs) index counter = do
    if counter == index
        then x
    else
        selectItemInInventory xs index (counter + 1)

-- Tells the Player that the pressed key is invalid and closes inventory
invalidOption :: X -> Y -> Character -> IO ()
invalidOption x y player = do
    setSGR [SetColor Foreground Vivid Red]
    slowTextRec "Invalid option.\n" 20000
    setSGR []
    slowTextRec "You close your bag and continue your adventure.\n" 20000
    battleCry player x y

-- Change player's attacked based on current and newweapon
changePlayerWeapon :: Character -> Item -> Int -> Character
changePlayerWeapon player item numOption
    -- New weapon has either more attack or less than the current one
    | weaponDamage item >= (weaponDamage $ weapon player) || weaponDamage item <= (weaponDamage $ weapon player) = do
        let newName = name player
        let newHealth = maxHealth player
        let newMaxHealth = maxHealth player
        let newCrit = crit player
        -- Add additional weapon damage to current attacks
        let newAttacks = changeWeaponDamage (attacks player) 10 (weaponDamage $ weapon player) (weaponDamage item)
        -- Put current weapon to bag
        let updatedInventory = inventory player ++ [weapon player]
        -- Put item from bag to hand and store the old item in the bag
        let newInventory = moveItemsPlayer updatedInventory numOption 0
        
        let newWeapon = item
        let newGold = gold player

        let newChar = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold

        newChar
    -- New weapon has equal attack to the currently equiped weapon
    | otherwise = player
    
-- Changes the player's damage
changeWeaponDamage :: Spells -> Attack -> Attack -> Attack -> Spells
changeWeaponDamage [] _ _ _= []
changeWeaponDamage (x:xs) spellDamage oldWeaponDmg newWeaponDmg = do
    [(fst x, (snd x + (newWeaponDmg - oldWeaponDmg) * spellDamage))] ++ changeWeaponDamage xs (spellDamage + 5) oldWeaponDmg newWeaponDmg
    
-- Remove selected item from bag - enemy
moveItemsEnemy :: Bag -> Item -> Bag
moveItemsEnemy [] _ = []
moveItemsEnemy (x:xs) item
    | x == item = moveItemsEnemy xs item
    | otherwise = [x] ++ moveItemsEnemy xs item

moveItemsPlayer :: Bag -> Int -> Int -> Bag
moveItemsPlayer [] _ _ = []
moveItemsPlayer (x:xs) numOption counter
    | counter == numOption = moveItemsPlayer xs numOption (counter + 1)
    | otherwise = [x] ++ moveItemsPlayer xs numOption (counter + 1)
    
-- Take the enemies items
lootEnemy :: Character -> Character -> X -> Y -> IO()
lootEnemy player enemy x y = do
    let time = 20000

    let text = "You inspect "++ (name enemy) ++".\n"
    slowTextRec text time
    
    setSGR [SetColor Foreground Dull Yellow]
    let text = name enemy ++ "'s Gold: " ++ (show $ gold enemy) ++ "\n"
    -- Display enemy's gold
    slowTextRec text 20000
    setSGR []

    -- Check enemy's inventory
    if (length $ inventory enemy) > 0
        then do
            -- Show enemy's inventory
            slowTextRec "Inventory: \n" time
            showInventory (inventory enemy) 0
            -- Options
            setSGR [SetColor Foreground Dull Yellow]
            slowTextRec "Type gold to take the enemy's gold.\n" time
            setSGR [SetColor Foreground Dull Green]
            slowTextRec "Type item to take an enemy's item.\n" time
            setSGR []
            slowTextRec "Type leave to stop inspecting the corpse. \n" time

            putStr "Loot: "
            option <- getLine
            putStrLn ""

            lootOption player enemy option x y
    -- Inventory is empty
    else do
        -- Check if enemy has gold
        if gold enemy > 0
            then do
            setSGR [SetColor Foreground Dull Red]
            slowTextRec "Inventory is empty.\n" time
            setSGR []

            setSGR [SetColor Foreground Dull Yellow]
            slowTextRec "Type gold to take the gold.\n" time
            setSGR []
            slowTextRec "Type leave to stop inspecting the corpse. \n" time

            putStr "Loot: "
            option <- getLine
            putStrLn ""
            
            lootEmptyInventory player enemy option x y
        -- Enemy neither has gold nor items
        else do
            setSGR [SetColor Foreground Dull Red]
            slowTextRec "Inventory is empty! \n\nEnemy does not have gold!\n" time
            setSGR []
            battleCry player x y

-- What to do if enemy's inventory is empty
lootEmptyInventory :: Character -> Character -> String -> X -> Y -> IO ()
lootEmptyInventory player enemy option x y
    | option == "gold" = lootGold player enemy x y
    | option == "leave" = do
        slowTextRec "You leave the dead body to rot and continue your adventure.\n" 20000
        battleCry player x y
    | otherwise = lootEnemy player enemy x y

-- What to do after user's input
lootOption :: Character -> Character -> String -> X -> Y -> IO ()
lootOption player enemy option x y
    -- Take enemy's gold
    | option == "gold" = lootGold player enemy x y
    -- Take a specific item from the enemy
    | option == "item" = lootItem player enemy x y
    -- Leave enemy's corpse
    | option == "leave" = do
        slowTextRec "You leave the dead body to rot and continue your adventure.\n" 20000
        battleCry player x y
    | otherwise = lootEnemy player enemy x y

-- Take enemy's gold
lootGold :: Character -> Character -> X -> Y -> IO ()
lootGold player enemy x y
    -- Enemy has gold
    | gold enemy > 0 = do
        -- Update player
        let newName = name player
        let newHealth = health player
        let newMaxHealth = maxHealth player
        let newCrit = crit player
        let newAttacks = attacks player
        let newInventory = inventory player
        let newWeapon = weapon player
        -- Update gold
        let newGold = gold player + gold enemy

        let newPlayer = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold
        
        -- Update enemy
        let newName = name enemy
        let newHealth = maxHealth enemy
        let newMaxHealth = maxHealth enemy
        let newCrit = crit enemy
        let newAttacks = attacks enemy
        let newInventory = inventory enemy
        let newWeapon = weapon enemy
        -- Update gold
        let newGold = 0

        let newEnemy = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold
        
        let text = "You take " ++ (name enemy) ++ "'s gold.\n"
        slowTextRec text 20000

        lootEnemy newPlayer newEnemy x y
    -- Enemy does not have gold
    | gold enemy <= 0 = do
        let text = (name enemy) ++ " has no gold.\n"
        slowTextRec text 20000
        lootEnemy player enemy x y

-- Loot specific item
lootItem :: Character -> Character -> X -> Y -> IO ()
lootItem player enemy x y = do
    -- Check if inventory is full
    if (length $ inventory player) <= 9
        then do
            putStr "Select item index: "
            option <- getChar
            let index = charToNum option
            putStrLn "\n"
            -- Check if the index is valid
            if index /= -1
                then do
                    let item = selectItemInInventory (inventory enemy) index 0

                    -- Check if item exists in enemy's inventory
                    if item /= (Item "N/A" "N/A" 0.0 0)
                        -- Put item in player's inventory
                        then do
                            -- Update player
                            let newName = name player
                            let newHealth = health player
                            let newMaxHealth = maxHealth player
                            let newCrit = crit player
                            let newAttacks = attacks player
                            -- Update inventory
                            let newInventory = inventory player ++ [item]
                            let newWeapon = weapon player
                            let newGold = gold player 

                            let newPlayer = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold

                            -- Update enemy
                            let newName = name enemy
                            let newHealth = maxHealth enemy
                            let newMaxHealth = maxHealth enemy
                            let newCrit = crit enemy
                            let newAttacks = attacks enemy
                            -- Update inventory
                            let newInventory = moveItemsEnemy (inventory enemy) item
                            let newWeapon = weapon enemy
                            let newGold = gold enemy

                            let newEnemy = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold

                            let text = "You loot: " ++ (weaponName item) ++ ".\n"
                            slowTextRec text 20000
                            putStrLn ""
                            lootEnemy newPlayer newEnemy x y

                    -- Item does not exist
                    else 
                        invalidOption x y player
            -- Invalid option
            else do
                invalidOption x y player
    else do
        setSGR [SetColor Foreground Dull Red]
        slowTextRec "Inventory is full!" 20000
        setSGR []
        lootEnemy player enemy x y

-- User interacts with vendor
vendor :: Character -> X -> Y -> IO ()
vendor player x y = do
    -- Display options
    vendorInputOptions
    -- Get user's input
    putStr "Option: "
    option <- getLine
    putStrLn ""

    -- Check if input is valid
    if option == "sell" || option == "buy" || option == "leave"
        then do
            vendorOption player x y option
    -- Input was invalid
    else do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Invalid option!\n"
        setSGR []
        vendor player x y

-- Check selected option
vendorOption :: Character -> X -> Y -> String -> IO()
vendorOption player x y option
    -- Sell items
    | option == "sell" && (length $ inventory player) > 0 = do
        let time = 20000
        slowTextRec "Inventory: \n" time
        showInventory (inventory player) 0
        
        -- Get user's input
        putStr "Enter item index: "
        index <- getChar
        putStrLn "\n"
    
        let option = charToNum index

        -- Check if given output is valid
        if option /= -1
            then do
                -- Get selected item
                let item = selectItemInInventory (inventory player) option 0

                -- Check if item is returned
                if item /= Item "N/A" "N/A" 0.0 0
                    then do
                        let newName = name player
                        let newAttacks = attacks player
                        let newCrit = crit player
                        let newHealth = health player
                        let newMaxHealth = maxHealth player
                        let newWeapon = weapon player
                        -- Add gold
                        let newGold = gold player + money item
                        -- Update inventory
                        let newInventory = moveItemsPlayer (inventory player) option 0

                        let newChar = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold
                        
                        setSGR [SetColor Foreground Vivid Yellow]
                        let text = "Item sold!\n\nGold: " ++ (show newGold) ++ "\n"
                        putStrLn text
                        setSGR []
                        -- Go back go vendor options
                        vendor newChar x y
                        
                -- Item not found in inventory
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn "Item not found!\n"
                    setSGR []
                    vendorOption player x y "sell"
        -- Input was invalid
        else do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Invalid option!\n"
            setSGR []
            vendorOption player x y "sell"
    -- Buy items
    | option == "buy" && (length $ inventory player) < 9 = do
        let vendorBag = [Item "Fel Blademaster Sword" "A fel blade, gift from the Burning Legion" 5.5 60, Item "Ressin's Sword" "Enhanced by the almighty Theory of Computation gods!" 7.0 100, Item "Thunderfury, Blessed Blade of the Windseeker" "The legendary sword once wielded by Thunderaan, Prince of Air. The prince, son of Al'Akir the Windlord, was attacked by Ragnaros the Firelord, in an attempt to heighten the already impressive power that the fire elemental held. Ragnaros succeeded; however, Thunderaan's power could not be completely taken into his form. What remained of Thunderaan was placed in a talisman of elemental binding, which was broken into two pieces. The pieces were then given to the Firelord's two lieutenants, Baron Geddon and Garr, respectively the left and right halves. These two halves are known as the  Bindings of the Windseeker." 20 1000]
        -- Display vendor's items
        putStrLn "Vendor's offer:\n"
        showInventory vendorBag 0

        -- Get user's input
        putStr "Select item: "
        index <- getChar
        putStrLn "\n"
        
        let option = charToNum index

        -- Check if given output is valid
        if option /= -1
            then do
            -- Get selected item
            let choice = selectItemInInventory vendorBag option 0
            
            -- Check if item is returned
            if choice /= Item "N/A" "N/A" 0.0 0
                then do
                    -- Check if player has enough money
                    if (gold player) >= (money choice)
                        then do
                            -- Reduce item's gold
                            let item = Item (weaponName choice) (description choice) (weaponDamage choice) (money choice * 0.7)
                            -- Modify player
                            let newName = name player
                            let newAttacks = attacks player
                            let newCrit = crit player
                            let newHealth = health player
                            let newMaxHealth = maxHealth player
                            let newWeapon = weapon player
                            -- Remove gold
                            let newGold = (gold player) - (money item)
                            -- Update inventory
                            let newInventory = inventory player ++ [item]

                            let newChar = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold
                            
                            setSGR [SetColor Foreground Vivid Yellow]
                            let text = (weaponName item) ++" was bought!\n\nRemaining gold: " ++ (show newGold) ++ "\n"
                            putStrLn text
                            setSGR []
                            -- Go back go vendor options
                            vendor newChar x y
                    else do
                        setSGR [SetColor Foreground Vivid Red]
                        putStrLn "Not enough gold!\n"
                        setSGR []
                        vendor player x y
            -- Item not found in inventory
            else do
                setSGR [SetColor Foreground Vivid Red]
                putStrLn "Item not found!\n"
                setSGR []
                vendorOption player x y "buy"
        -- Option is not valid
        else do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Invalid option!\n"
            setSGR []
            vendor player x y
    -- Inventory is full
    | option == "buy"  && (length $ inventory player) >= 9 = do
        setSGR [SetColor Foreground Vivid Red]
        slowTextRec "Inventory is full!\n" 20000
        vendor player x y
    -- Inventory is empty
    | option == "sell" && (length $ inventory player) == 0 = do
        setSGR [SetColor Foreground Vivid Red]
        slowTextRec "Inventory is empty!\n" 20000
        vendor player x y
    -- Leave store
    | otherwise = do
        slowTextRec "You leave the vendor and continue your adventure!\n" 20000
        action x y player
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
action :: X -> Y -> Character -> IO ()
action x y player = do

    -- Get user desired location
    putStr ("Action: ")
    actionInput <- getLine 
    putStrLn ""

    if actionInput /= ""
        then do
            let currentAction = removeMaybe $ stringToAction $ toUp actionInput
            -- Check if input was valid
            if currentAction == Exit
                then action x y player
            else do
                -- Check if inventory was selected
                if currentAction == Inventory
                    -- Display inventory
                    then 
                        inventoryOption player x y
                
                else do
                    -- Move inside grid
                    let newCord = move currentAction x y
                    
                    -- Display a random move quote
                    randomMoveQuote (getRandomNumber 1 4) 20000

                    -- Check what has happened to the player
                    let nextAction = checkNextAction (fst newCord) (snd newCord)

                    checkPlayerStatus nextAction (fst newCord) (snd newCord) player
    else action x y player

    return ()

-- Convert the String movement to a Maybe Movement data type
stringToAction :: String -> Maybe Action
stringToAction "North" = Just North
stringToAction "South" = Just South
stringToAction "East" = Just East
stringToAction "West" = Just West
stringToAction "Exit" = Just Exit
stringToAction "Inventory" = Just Inventory
-- If player misspelt a word
stringToAction _ = Nothing

-- Remove Maybe from Movement data type
removeMaybe :: Maybe Action -> Action
removeMaybe Nothing = Exit
removeMaybe (Just action) = action

-- Move position within grid
move :: Action -> X -> Y -> Position
move action x y 
    | action == North = (x, decPos y)
    | action == South = (x, incPos y)
    | action == East = (incPos x, y)
    | action == West =  (decPos x, y)

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
    -- Bumped into a vendor
    | (x, y) == (0, 2)
        || (x, y) == (6, 1)
        || (x, y) == (6, 3) = Vendor
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
        finish
        setSGR []
    -- Check if the player is in combat
    | status == Combat = do
        let enemy = getEnemy (getRandomNumber 1 3)
        healthStatus player enemy x y
    | status == Boss = do
        let boss = Character "Pit Lord" [(("1) Slam", "Slams the opponent."), 10.00), (("2) Fel Fire Nova", "Emitting a steady pulse of fel fire, dealing damage to the player."), 50.00), (("3) Overpower", "Instantly overpower the enemy, causing high weapon damage."), 70.00)] 3.0 1000.00 1000.00 [ Item "Gorehowl" "The mighty weapon of the Hellscream family" 10.00 1000] (Item "Gorehowl" "The mighty weapon of the Hellscream family" 10.00 1000) 100
        healthStatus player boss x y
    -- Check if player has reached North map end 
    | status == NorthBack = do
        endOfMap
        action x (incPos y) player
    -- Check if player has reached South map end
    | status == SouthBack = do
        endOfMap
        action x (decPos y) player
    -- Check if player has bumped into a Fel river
    | status == FelDrink = felBlood player x y
    -- Check if player has reached Fel lava - go back (x - 1, y)
    | status == FelLava = do
        felLava
        action (decPos x) y player
    -- Check if player has reached the initial Fel lava - (0, 0) - go to (0, 1)
    | status == FelLavaStart = do
        endOfMap
        action x (incPos y) player
    -- Check if player has reached West map end
    | status == WestBack = do
        endOfMap
        action (incPos x) y player
    -- Check if player has met a vendor on their wya
    | status == Vendor = do
        vendorDialogue player
        vendor player x y
    -- Check if player is alive and can freely move
    | status == Walk = action x y player
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
        action x y player
    -- Check if player is on the right side of the Fel Fire wall
    | status == FelFireWallRight = do
        felFireWallBump
        action (incPos x) y player
    -- Check if player is on the left side of the Fel Fire wall
    | status == FelFireWallLeft = do
        felFireWallBump
        action (decPos x) y player
    -- Check if player has bumped into a person
    | status == Person = do
        person (getRandomNumber 1 3) 20000
        action x y player
    -- Check if player has finished the game
    | status == Finish = finish

-- Generates a mob for the player to fight
getEnemy :: Int -> Character
getEnemy n 
    | n == 1 = Character "Felguard" [(("1) Axe Toss", "The felguard hurls his axe"), 30.00), (("2) Legion Strike", "A sweeping attack that does damage"), 35.00), (("3) Overpower", "Charge the player and deal damage."), 40.00)] 1 300.00 300.00 [Item "Felguard Axe" "A mighty axe, forged by Sargeras himself." 3.1 20] (Item "Felguard Axe" "A mighty axe, forged by Sargeras himself." 3.1 20) 30
    | n == 2 = Character "Imp" [(("1) Felbolt", "Deal fel fire damage to the player"), 10.00), (("2) Scorch", "Burn the player with fel fire"), 15.00), (("3) Rain of Fel", "Fire a fel meteorite shower"), 30.00)] 0.2 150.00 150.00 [Item "Fel sword" "A tiny sword full of anger." 0.05 5] (Item "Fel sword" "A tiny sword full of anger." 0.05 10) 10
    | n == 3 = Character "Voidlord" [(("1) Void grab", "Deal void damage to the player "), 30.00), (("2) Void bolt", "Send a void bolt to the player "), 35.00), (("3) Drain soul", "Drain the player's soul"), 50.00)] 0.8 200.00 200.00 [Item "Void claws" "Sharp and deadly claws." 2.0 15] (Item "Void claws" "Sharp and deadly claws." 2.0 15) 20

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
    let newInventory = inventory player
    let newWeapon = weapon player
    let newGold = gold player
    let newChar = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold
    
    dialogue "Samuro" "For the Burning Blade!\n" 20000
    dialogue "Battle cry" "Increase overall damage by 0.5% for all of your spells!\n" 20000
    slowTextRec "Your wounds magically heal faster. You have been restored to maximum health.\n" 20000

    -- Move character
    action x y newChar

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
        let newInventory = inventory player
        let newWeapon = weapon player
        let newGold = gold player

        let newChar = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold
        setSGR [SetColor Foreground Vivid Green]
        slowTextRec "You have accepted Gul'dan's offer. Now you feel stronger!\n" 20000

        action x y newChar
    | choice == "no" = do
        slowTextRec "You have rejected Gul'dan's gift.\n" 20000
        action x y player
    | otherwise = felBlood player x y

-- Check where the player's health is
checkHealth :: Health -> Health -> Health -> Health
checkHealth old current new
    -- Check if player's current health is between the old max health and new max health
    | old >= current && current >= new = current - (current - new)
    -- check if player's current health is lesser or equal to the new health
    | new >= current = current

-- Increase player's health 3 times
narusGift :: Character -> X -> Y -> IO ()
narusGift player x y = do
    let newName = name player
    let newCrit = crit player 
    let newAttacks = attacks player
    -- Increase maximum health 3 times
    let newMaxHealth = maxHealth player * 3
    -- Heal player to maximum health
    let newHealth = newMaxHealth
    let newInventory = inventory player
    let newWeapon = weapon player
    let newGold = gold player
    let newChar = Character newName newAttacks newCrit newHealth newMaxHealth newInventory newWeapon newGold
    -- Naru's gift info
    dialogue "Naru's gift" "Heals you and tripples your health.\n" 20000
    action x y newChar