module GameLogic where

type Coord = (Int, Int)
type Move = [Coord]

data StudentGame = StudentGame [Coord] [Coord] [Coord] [Coord] Player Player Status String
  deriving (Show, Eq)
{-
VERY IMPORTANT:
  - First list of coordinates is the position of the black pieces
  - Second list of coordinates is the position of red pieces
  - Third list of coordinates is the position of black kings
  - Fourth list of coordinates is the position of red kings
  - First Player is Black Player (Human or CPU)
  - Second Player is RedPlayer (Human or CPU)
  - Status: RedTurn, BlackTurn, GameOver, NewGame
  - String: Message for the players (e.g. invalid move)
-}
data Status = Red | Black | GameOver | NewGame
  deriving (Show, Eq)
data Player = Human | CPU AI
  deriving (Show, Eq)
{-
  Students may add their own AI's, and test them against eachother.
  This feature will be expanded as the semester continues.
-}
data AI = Default | Alt 
  deriving (Show, Eq, Enum)

 

{-
Here are the function signatures the students need to fill in.
-}

studentApplyMove :: Move -> StudentGame -> StudentGame
studentApplyMove _ x = x

studentGetMove :: StudentGame -> Move
studentGetMove _ = []

 
