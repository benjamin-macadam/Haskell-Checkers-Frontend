# System

This provides a front-end for a checkers game written in Haskell
using the Brick library. It is meant for UCalgary CPSC449 course in
programming paradigms.

## Setting up and running the program

To set up the program, you must have 
[stack installed](https://www.haskellstack.org "stack-download-link")
(click on the above link to download and install stack). Then clone this
repository go into the director and run:

``` shell
stack build
```
stack will then setup an appropriate environment to run this project.
To run this program, enter

``` shell
stack run
```

and the program will run (note that at this point, all that will happen
is a simple human vs. human game will be initiated).


## Game Loop

The game loop is split into two parts, human and CPU.

### Human move

The human controls the cursor and builds a move. This is activated
when the gamestate has a "Human" player making a move.

-   pressing the arrow keys navigates the board
-   pressing the space key adds the current square to the move.
-   pressing enter "applies the move".
-   if the move is legal, then the move should be applied and the next players turn begins.
-   if the move is illegal, then the game must tell them they made an illegal move and ask them to try again.

### CPU move

When CPU's move will be displayed in the "move" bracket of the game status menu. 
The user must press enter for the move to be applied.

## Building the Software


### Important types

Here is a list of important types to build the checkers game.

``` haskell
type Coord = (Int, Int)

type Move = [Coord]

data Status = Red | Black | GameOver 
  deriving (Show, Eq)

```

Each square of the checkers board is represented by a pair of integers,
and a move is represented by a list of coordinates. The status determines 
whose turn it is (or if the game has already been won).

``` haskell
data GameState =
  GameState { _blackPieces :: [Coord]
            , _redPieces :: [Coord]
            , _blackKings :: [Coord]
            , _redKings :: [Coord]
            , _status :: Status
            , _message :: String}
              deriving (Show, Eq)


```

In the gamestate, the coordinates of red and black pieces are recorded in the pieces and kings lists.
The status determines whose turn it is (or if it is game over).
The message instructs the player on what to do (i.e. if they made an illegal move they must try again).

The gamestate data type is a [lens](https://github.com/ekmett/lens/wiki "lens-wiki-link"). 
You may manipulate it using [record syntax](https://en.wikibooks.org/wiki/Haskell/More_on_datatypes "record syntax") or by using the set, view, and over functions.

``` haskell
set blackpieces [] g -- update using lens
g & blackpieces .~ [] -- update using lens infix notation
g{_blackpieces = []_} -- update using record notation

view blackpieces g -- view using lens
g^.blackpieces     -- view using lens infix notation
_blackpieces g     -- view using record notation

```

Here are some examples on how the different syntaxes are used for basic commands.
We recommend you pick one and stay consistent throughout your code!

``` haskell
type ApplyMove = Move -> GameState -> GameState

type CpuMove = GameState -> Move

```
The applymove and CpuMove types are very important - an ApplyMove will provide "gamelogic" for your
checkers game, and a CpuMove will be an AI player.


### Important functions

In haskell, the main function has type IO().
We provide four functions:

``` haskell
humanTui :: ApplyMove -> GameState -> IO ()
humanTui = generalConstructor Human Human

redAiTui :: CpuMove -> ApplyMove -> GameState -> IO ()
redAiTui r = generalConstructor (CPU r) Human

blackAiTui :: CpuMove -> ApplyMove -> GameState -> IO ()
blackAiTui b = generalConstructor Human (CPU b)

aiTestTui :: CpuMove -> CpuMove -> ApplyMove -> GameState -> IO ()
aiTestTui r b = generalConstructor (CPU r) (CPU b)
```
 
To run the program, you will need to go to Main.hs and set

``` haskell
main = yourChoiceOfTui args
```

You will need to hand the program the AI programs, game logic function, and the gamestate you wish
to begin the game from (you may hand the game state to more easily test your code).
We have included the convenience function providing the initial game state.

``` haskell
initialGameState :: GameState
initialGameState =
  GameState { _blackPieces = blackInit
            , _redPieces = redInit
            , _blackKings = []
            , _redKings = []
            , _status = Red
            , _message = ""}

```
