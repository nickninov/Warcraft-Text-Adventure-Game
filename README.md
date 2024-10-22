# Haskell-Warcraft-Text-Adventure-Game
A Warcraft text adventure game made with Haskell. This game was inspired by the Warcraft 3 quest **[Infiltrate Theramore](https://youtu.be/8AXsjEg9WNM?t=292)** where you play as Samuro

## Files

### main.hs
Launches the game

### DataTypes.hs
Consists of all the data types used within the game.

### Effects.hs
Consists of all the text effects that are used in the game

### Actions.hs
Consists of all the movement logic, combat system and stats modifiers in the game

### Functions.hs
Consists of all the generic functions, that are used within the game and dealing with saving or loading the game

## Used libraries

- [System.Random](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html) <br>`cabal install random --lib`
- [System.Console.ANSI](https://hackage.haskell.org/package/ansi-terminal-0.10.3/docs/System-Console-ANSI.html) <br>`cabal install ansi-terminal --lib`

## How to run
- `:load main.hs`
- `main`

## How to play
- Type north, south, east or west to move around the map
- Type inventory to open you bag. You can equip items you loot from enemies or you have bought.
- Whilst walking you may encounter different difficulty enemies. Press 1, 2, 3 or 4 to cast a spell on an enemy.
