-- Main file to load the game!

-- Import files
import Effects
import Functions
import DataTypes
import Actions

-- Import libraries
import System.IO

main :: IO ()
main = do
    -- Initial starting details
    let x = 1
    let y = 1
    let health = 200.00
    let crit = 0.5
    let inventory = [Item "Malte's Axe" "Forged by the Pumping Lemma of Context Free Languages" 1.0 100]
    let item = Item "Blademaster Sword" "A traditional blade from the Burning Blade clan." 3.0 10
    let weaponDmg = weaponDamage $ item
    let spells = [(("1) Heroic Strike", "A strong attack that increases melee damage. Can be casted all the time."), weaponDmg * 10), (("2) Mortal Strike", "A vicious strike that deals weapon damage. Can be casted on enemies below 70% health"), weaponDmg * 15), (("3) Bladestorm", "Become an unstoppable storm of destructive force. Can be casted on enemies below 50%"), weaponDmg * 20), (("4) Execute", "Attempt to finish off a wounded foe. Can be casted to enemy below 20%."), weaponDmg * 25) ]
    let gold = 0
    let character = Character "Samuro" spells crit health health inventory item gold

    -- Intro effect - imported from Effects.hs
    intro

    -- Start moving in the game - imported from Movement.hs
    action x y character

    return ()