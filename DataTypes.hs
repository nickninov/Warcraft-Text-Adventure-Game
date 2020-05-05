-- This file contains all the different data types that are defined within the game
module DataTypes where

-- Game Types

-- Coordinate types
type X = Int
type Y = Int
type Position = (X, Y)
type Positions = [Position]

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
data Action = South | East | West | North | Inventory | Leave | Exit
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