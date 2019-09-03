{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
module GameLogic where

import Lens.Micro.Platform


type ApplyMove = Move -> GameState -> GameState

type AiMove = GameState -> Move

data MoveType = Human | AI AiMove

type Coord = (Int, Int)
type Move = [Coord]

data Status = Red | Black | GameOver 
  deriving (Show, Eq)


data GameState =
  GameState { _blackPieces :: [Coord]
            , _redPieces :: [Coord]
            , _blackKings :: [Coord]
            , _redKings :: [Coord]
            , _status :: Status
            , _message :: String}
              deriving (Show, Eq)

makeLenses ''GameState


initialGameState :: GameState
initialGameState =
  GameState { _blackPieces = blackInit
            , _redPieces = redInit
            , _blackKings = []
            , _redKings = []
            , _status = Red
            , _message = ""}

blackInit :: [Coord]
blackInit = [ (1,0), (3,0), (5,0), (7,0)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,2), (3,2), (5,2), (7,2)]

redInit :: [Coord]
redInit = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (0,5), (2,5), (4,5), (6,5)]

setMessage :: GameState -> GameState
setMessage s = case (s^.status) of
  Red -> set message
    "Red Turn." s
  Black -> set message
    "Black Turn." s
  _ -> s
applyMove :: Move -> GameState -> GameState
applyMove _  s = case s^.status of
  Red -> setMessage $ set status Black s
  Black -> setMessage $ set status Red s
  _ -> initialGameState

 
