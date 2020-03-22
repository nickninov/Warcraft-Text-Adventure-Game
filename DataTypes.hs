-- This file contains all the different data types that are defined within the game
module DataTypes where

-- Game Types

-- Coordinate types
type X = Int
type Y = Int
type Position = (X, Y)

-- Player types
type Name = String
type Attack = Double
type Crit = Double
type Health = Double
type Description = String
type Option = Char
type Spells = [((Name, Description), Attack)]
type AttackStatus = (Bool, Attack)
type Bag = [Item]
type Gold = Double


-- Data Types

-- Action data type - actions that the user can do
data Action = South | East | West | North | Inventory| Exit
    deriving (Show, Read, Eq)

-- Character data type 
data Character = Character {
    name :: Name,
    attacks :: Spells,
    crit :: Crit,
    health :: Health,
    maxHealth :: Health,
    inventory :: Bag,
    weapon :: Item,
    gold :: Gold
} deriving (Show, Read, Eq)

-- Item data type
data Item = Item {
    weaponName :: Name,
    description :: Description,
    weaponDamage :: Attack,
    money :: Gold
} deriving (Show, Read, Eq)

{- 
    Status data type - is the character dead, alive, in combat or in a room.
    Dead - the player is dead
    Walk - the player is alive and can move
    Combat - the player is in combat mode
    Vendor - the player has met a vendor to sell and buy items
    Boss - the player is fighting against a boss
    Portal - the player bumps into a portal and gets teleported
    Finish - the player has successfully completed the game
    Corpse - the player bumps into another fallen hero
    Person - the player has met a person
    Arechron - the player has met Arechron
    NorthBack & WestBack & SouthBack - the player has reached the end of the map and is visiting a cave.
    FelFireWallLeft - the player has reached a fel fire wall from the left side
    FelFireWallRight - the player has reached a fel fire wall from the right side
    FelLava - the player has reached a green lava pool
    FelLavaStart - the player has bumped into the initial Fel Lava - (0, 0)
    FelDrink - the player can drink the blood of Mannoroth
-}
data CharacterStatus = Dead | Walk | Combat | Vendor
            | Boss | Portal | Finish | Corpse | Person | Arechron
            | NorthBack | WestBack | SouthBack 
            | FelFireWallLeft | FelFireWallRight | FelLavaStart | FelLava | FelDrink
    deriving (Show, Read, Eq)