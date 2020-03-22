-- This file contains all the different text effects within the game
module Effects where

-- Import libraries
import System.IO
import Control.Concurrent
import System.Console.ANSI

-- Import files
import DataTypes

-- Display text with the given delay time (microseconds)
slowTextRec :: String -> Int -> IO ()
slowTextRec [] _ = do 
    setSGR []
    putStrLn ""
slowTextRec string time = do
    putStr [(head string)]
    threadDelay time
    slowTextRec (tail string) time

-- Display green dialogue
dialogue :: String -> String -> Int -> IO ()
dialogue person text time = do
    putStr person
    putStr ": "
    setSGR [SetColor Foreground Vivid Green]
    slowTextRec text time

-- Show character's health
showHealth :: Character -> IO ()
showHealth character = do
    -- Display character's name
    putStr (name character)
    putStr "'s health: "

    -- Calculate health in percentage
    let hpPercent = (health character / maxHealth character) * 100

    -- Display character's health
    showHealthStatus character hpPercent

-- Show character's health in different colours
showHealthStatus :: Character -> Double -> IO()
showHealthStatus character hpPercent
    -- Green health - from max health to half health
    | health character <= maxHealth character && health character >= maxHealth character / 2 = do
        setSGR [SetColor Foreground Vivid Green]
        putStr $ show $ round $ health character
        putStr " ("
        putStr $ show $ round hpPercent
        putStrLn "%)"
        setSGR []
    -- Red health - for health lower than half
    | otherwise = do
        setSGR [SetColor Foreground Vivid Red]
        putStr $ show $ round $ health character
        putStr " ("
        putStr $ show $ round hpPercent
        putStrLn "%)"
        setSGR []

-- Display quest with yellow colour
quest :: String -> String -> Int -> IO ()
quest person text time = do
    setSGR [SetColor Foreground Dull Yellow]
    putStr person
    putStr ": "
    slowTextRec text time
    setSGR []

-- Display yellow spell ability
showSkills :: Name -> Description -> String -> Int -> IO ()
showSkills skill text dmg time = do
    putStr skill
    putStr ": "
    setSGR [SetColor Foreground Vivid Yellow]
    slowTextRec text time
    showDmg dmg time

-- Display red ability's damage
showDmg :: String -> Int -> IO ()
showDmg dmg time = do
    putStr "Damage: "
    setSGR [SetColor Foreground Vivid Red]
    slowTextRec dmg time

-- A random quote that the character will say when they move
randomMoveQuote :: Int -> Int -> IO ()
randomMoveQuote rng time
    | rng == 1 = dialogue "Samuro" "I hear and obey.\n" time
    | rng == 2 = dialogue "Samuro" "Hui!\n" time
    | rng == 3 = dialogue "Samuro" "Yes!\n" time
    | rng == 4 = dialogue "Samuro" "Excellent choice.\n" time

-- Diallgue introduction - imported inside main.hs
intro :: IO ()
intro = do
    let time = 20000
    setTitle "21361642's Warcraft Text Adventure Game"

    slowTextRec "21361642's Warcraft Text Adventure Game\n" time
    slowTextRec "You are have been teleported to a different realm, conquered by the Burning Legion! You need to find your way back to Durotar and help Rexxar sneak accross the channel in his boat.\n" time
    
    dialogue "Samuro" "Throm-Ka player! I am Samuro. My humble skills are yours.\n" time
    
    putStrLn "How to play:\n"
    dialogue "Movement" "Type north, south, east or west to move around the map.\n" time
    dialogue "Inventory" "Type inventory to open you bag. You can equip items you loot from enemies.\n" time
    dialogue "Combat" "Whilst walking you may encounter different difficulty enemies. Press 1, 2, 3 or 4 to cast a spell on an enemy.\n" time
    dialogue "Interaction" "You may encounter with other people, trapped in this realm or corpses of fallen heroes.\n" time
    quest "Quest" "Find a portal to Durotar!\n" time
    dialogue "Tip" "Go south.\n" time

-- Credits to the game creator
credits :: Int -> IO ()
credits time = do
    slowTextRec "\nThank you for playing!" time
    putStrLn ""
    dialogue "Game creator" "Nikolay Nikolaev Ninov" time
    dialogue "Course lecturer" "Malte Ressin" time
    dialogue "Inspiration" "Warcraft Lore" time
    dialogue "Github" "nickninov" time

-- Diallgue for successfully completed the game - imported inside Movement.hs
finish :: IO ()
finish = do
    let time = 20000
    setSGR [SetColor Foreground Vivid Yellow]
    slowTextRec "Quest completed!\n" time
    slowTextRec "You have successfully completed the Warcraft Text Adventure Game!" time
    setSGR []
    credits time

-- Display text with the given delay time (microseconds) for dying
deathText :: String -> Int -> IO ()
deathText [] _ = do 
    setSGR []
    putStrLn ""
deathText string time = do
    putStr [(head string)]
    threadDelay time
    slowTextRec (tail string) time

-- Diallgue for dying by an enemy
killedByEnemy :: Character -> IO ()
killedByEnemy enemy = do
    setSGR [SetColor Foreground Vivid Red]
    let time = 20000
    let text = "You have been slain in combat by "++(name enemy)++"!\nThis is the most honorable death for an Orc!\nGame over!"
    deathText text time
    credits time

-- Diallgue for killing enemy
killedEnemy :: Character -> Character -> X -> Y -> IO ()
killedEnemy player enemy x y = do
    setSGR [SetColor Foreground Vivid Red]
    let time = 20000
    let text = name enemy ++ " was slain by "++ name player ++".\n"
    deathText text time
    setSGR []

-- Diallgue for Fel blood
felOffer :: IO ()
felOffer = do
    let time = 20000
    setSGR [SetColor Foreground Vivid Green]
    slowTextRec "You have bumped into a Fel blood pool. Drink the blood of the demons and your overall damage and critical strike will increase by 50%.\n" time
    dialogue "Side effect" "You lose 10% of your maximum health\n" time
    setSGR [SetColor Foreground Vivid Green]
    slowTextRec "Type yes to accept the offer.\n" time
    setSGR [SetColor Foreground Vivid Red]
    slowTextRec "Type no to decline the offer\n" time
    setSGR []

-- Diallgue for reaching Fel lava
felLava :: IO ()
felLava = do
    let time = 20000
    dialogue "Samuro" "I have reached Fel lava. This does not look like the way to go. I need to go back and find a bridge to cross.\n" time

-- Diallgue for reaching initial Fel lava - (0, 0)
endOfMap :: IO ()
endOfMap = do
    let time = 20000
    dialogue "Samuro" "I have reached the end of this realm. There seems to be nothing there. I need to go back and find the portal.\n" time
    slowTextRec "All you can see are the countless worlds, invaded by the Burning Legion. You head back.\n" time

-- Diallgue for reaching a portal
portal :: IO ()
portal = do
    let time = 20000
    slowTextRec "You have been randomly teleported in the next zone. An enemy attacks you from behind.\n" time

-- Diallgue for entering combat with a boss
bossIntro :: IO ()
bossIntro = do
    let time = 20000
    slowTextRec "In front of you see an enormous demon charging furiously at you!\n" time

-- Diallgue for seeing a corpse
corpse :: IO ()
corpse = do
    let time = 20000
    slowTextRec "As you are walking you see a slain fellow orc.\n" time
    dialogue "Samuro" "You will be avenged fallen brother!\n" time
    slowTextRec "You pay respect to the fallen orc.\n" time

-- Dialogue for reaching the Fel Firewall from right the right side
felFireWallBump :: IO ()
felFireWallBump = do
    let time = 20000
    dialogue "Samuro" "It seems that I cannot go forward anymore. The Fel fire will burn me! Going back East\n" time
    slowTextRec "You go back.\n" time

-- Generate a random person encounter
person :: Int -> Int -> IO ()
person n time
    | n == 1 = do
        slowTextRec "As you are walking you bump into a half dead person.\n" time
        dialogue "Person" "They are so cold hearted. So cold. The life that had dwelt within me has gone. They killed my wife and daughter and left me to rot here until I die. No harm can come to me anymore.\n" time
        slowTextRec "The corpse was almost devoid of skin and pitted by burrowing insects. Horrified by the Burning Legion, you continue your journey.\n" time 
    | n == 2 = do
        slowTextRec "You see a burning hut in front of you. There is something standing in front of it, shivering and screaming nonsence. You decide to approach the thing.\n" time
        dialogue "Unknown" "The big bang isn't what they think... or it is, kinda. It's evidence, but they haven't probed behind it to understand what time is and how in a way, all matter is still right there in that infinitely small speck. You see, energy and matter are one and the same. Time and distance are also the same thing, hence \"spacetime\". When we imagine time as linear, and know that we occupy one moving moment of it, that's some progress. But reality is a little stranger Samuro.\n" time
        dialogue "Samuro" "WHO ARE YOU? HOW DO YOU KNOW ME?\n" time
        dialogue "Unknown" "The Burning Legion showed me the truth. I know everything that existed, exists and will exist.\n" time
        slowTextRec "Driven by its madness, the thing falls on the ground and dies.\n" time
        dialogue "Samuro" "I need to get out of here!\n" time
    | n == 3 = slowTextRec "As you are walking you see an endless sea of dead bodies. They lie like dolls over the battlefield, limbs at awkward angles. These bodies, once the repositories of people as alive as you are, are now abandoned shells left to rot in the open.\n" time

-- Dialogue with Arechron
dialogueArechron :: IO ()
dialogueArechron = do
        let time = 20000
        slowTextRec "As you are walking you see something in the distance.\n" time
        dialogue "Arechron" "Ah, hello champion! I have been watching you since you arrived on this realm and aiding!\n" time
        dialogue "Samuro" "At least you are alive. What is this place? Who are you?\n" time
        dialogue "Arechron" "I am Arechron, a Broken. We are a mutated Dranei race that was exposed to the Fel energies wielded by Orc Warlocks. You are currently in Outland - the shattered floating remnants of the destroyed world of Draenor. It was the homeworld of the Orcs and refuge of the Draenei. However the Burning Legion has managed to invade us and are teleporting champions from different worlds here to test their might. The only way to get out of here is just to head east. I have never met anybody as strong as you! I am giving you Naru's blessing! It will aid you in battle against the Pit Lord.\n" time
        dialogue "Samuro" "Lok'tar Archeron!\n" time
        dialogue "Arechron" "Good luck!\n" time

-- Punishment text for skipping a turn
punishmentText :: Character -> IO ()
punishmentText enemy = do
    setSGR [SetColor Foreground Vivid Red]
    let text = "You got distracted! " ++ name enemy ++ " attacked you!\n"
    let time = 20000
    slowTextRec text time
    setSGR []

-- Display inventory 
showInventory :: Bag -> Int -> IO ()
showInventory [] _ = return ()
showInventory (x:xs) counter = do
    let strCounter = show counter
    let strSpellName = weaponName x
    let text1 = strCounter ++ ") " ++ strSpellName ++ ":"
    
    slowTextRec text1 20000
    
    setSGR [SetColor Foreground Vivid Yellow]
    let text2 = (weaponName x) ++ " Deals " ++ (show $ round $ weaponDamage x) ++ " weapon damage."
    slowTextRec text2 20000
    setSGR []
    let text3 = "Price: " ++ (show $ money x) ++ " Gold\n"
    slowTextRec text3 20000
    showInventory xs (counter + 1)

-- Vendor interaction
vendorDialogue :: Character -> IO ()
vendorDialogue player = do
    let time = 20000
    let text = "Greetings "++ (name player) ++ "! Would you like to buy or sell? I have the finest goods from this realm!\n"
    dialogue "Unknown" text time

vendorInputOptions :: IO ()
vendorInputOptions = do
    setSGR [SetColor Foreground Vivid Yellow]
    slowTextRec "Type sell to sell items from your inventory.\n" 20000
    setSGR [SetColor Foreground Vivid Green]
    slowTextRec "Type buy to buy items from your inventory. \n" 20000
    slowTextRec "Type leave to leave the vendor.\n" 20000