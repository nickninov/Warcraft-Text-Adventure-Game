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
    let startPos = (1, 1)
    let health = 200.00
    let crit = 0.5
    let spells = [(("1) Heroic Strike", "A strong attack that increases melee damage."), 40.00), (("2) Mortal Strike", "A vicious strike that deals weapon damage."), 50.00), (("3) Bladestorm", "Become an unstoppable storm of destructive force."), 60.00)]
    let character = Character "Samuro" spells crit health health

    -- Intro effect - imported from Effects.hs
    intro

    -- Start moving in the game - imported from Movement.hs
    movement (fst startPos) (snd startPos) character

    return ()