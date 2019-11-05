{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module Checkers ( human
                , redAi
                , blackAi
                , aiTest
                , initialGameState
                , GameState(..)
                , Coord
                , Move
                , ApplyMove
                , AiMove
                , Status(..)
                )
where


import System.Directory
import Control.Monad.IO.Class
import Data.Maybe


--
import Lens.Micro.Platform

--
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Core
import Brick.Widgets.Border
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

import qualified Graphics.Vty as V


import Cursor.Simple.List.NonEmpty

import Data.List.NonEmpty (NonEmpty(..), toList)
import qualified Data.List.NonEmpty as NE
import System.Exit


import GameLogic as SC

{-
 Datatype Declarations:
We begin with the datatype declarations,keeping them
in a fixed area of the program for easy reference.

 The next task will involve translating these to
Lenses, and dealing with the changes as the
propograte throughout the code.
-}



type Board = NonEmptyCursor (NonEmptyCursor SC.Coord)

data TuiState =
  TuiState { _board :: Board
           , _move :: Move
           , _game :: GameState
           , _redMove :: MoveType
           , _blackMove :: MoveType
           , _moveLogic :: ApplyMove}

type ResourceName = String

{-
  Set up lenses
-}
makeLenses ''TuiState
makeLenses ''GameState
{-
  Cursor functions
-}

accessCursor :: Board -> SC.Coord
accessCursor = nonEmptyCursorCurrent . nonEmptyCursorCurrent

{-
  Main function
-}


human :: ApplyMove -> GameState -> IO ()
human = generalConstructor Human Human

redAi :: AiMove -> ApplyMove -> GameState -> IO ()
redAi r = generalConstructor (AI r) Human

blackAi :: AiMove -> ApplyMove -> GameState -> IO ()
blackAi b = generalConstructor Human (AI b)

aiTest :: AiMove -> AiMove -> ApplyMove -> GameState -> IO ()
aiTest r b = generalConstructor (AI r) (AI b)

generalConstructor :: MoveType -> MoveType -> ApplyMove -> GameState -> IO ()
generalConstructor r b a g = do
  let initialState = buildInitialState r b a g
  endState <- defaultMain tuiApp initialState
  print (endState^.game)
{-
  Basic tuiApp information.
-}

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const theAttributes}

theAttributes :: AttrMap
theAttributes = attrMap V.defAttr
  [(evenBlock, V.green `on` V.brightBlue)
  ,(oddBlock, V.green `on` V.white)
  ,(evenSel,  V.green `on` V.brightCyan)
  ,(oddSel, V.green `on` V.brightWhite)
  ,(redPiece, V.green `on` V.red)
  ,(blackPiece, V.green `on` V.black)
  ,(plain, V.black `on` V.white)]

-- Empty squares
evenBlock, oddBlock, evenSel, oddSel, redPiece, blackPiece :: AttrName
evenBlock = "evenBlock"
oddBlock = "oddBlock"
evenSel = "evenSel"
oddSel = "oddSel"
redPiece = "redPiece"
blackPiece = "blackPiece"
-- Squares with pieces

plain :: AttrName
plain = "plain"

-- Initial TUI
buildInitialState :: MoveType -> MoveType -> ApplyMove -> GameState -> TuiState
buildInitialState r b a g =
  TuiState { _board = initialBoard
           , _move = []
           , _game = g
           , _redMove = r
           , _blackMove = b
           , _moveLogic = a}

initialBoard :: NonEmptyCursor (NonEmptyCursor SC.Coord)
initialBoard = makeNonEmptyCursor $ NE.fromList
  [ makeNonEmptyCursor $
    NE.fromList [(i,j) | i <- [0..7] ] | j <- [0..7]]

{-
  Draw function.
-}

drawUI :: TuiState -> [Widget ResourceName]
drawUI s = case s^.game^.status of
  GameOver -> drawGameOverUI s
  _ -> drawGameUI s

drawGameUI :: TuiState -> [Widget ResourceName]
drawGameUI s =
  [ C.center $ padRight (Pad 2) (drawStats s) <=> drawGrid s ]

-- Game Over UI
drawGameOverUI :: TuiState -> [Widget ResourceName]
drawGameOverUI s = [ withBorderStyle BS.unicodeBold
                     $ B.borderWithLabel (str "Game Mode")
                     $ vBox [ withAttr plain $ str $ s^.game^.message
                            , withAttr plain $ str $
                              "Press n for a new game, q to quit"]]


-- Game Loop UI

drawStats :: TuiState -> Widget ResourceName
drawStats s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Game Info")
  $ vBox rows
  where
    rows = [ msgRow
           , statusRow
           , moveRow ]
    msgRow = withAttr plain $ str $ s^.game^.message
    statusRow = withAttr plain $ str $ show $ s^.game^.status
    moveRow = withAttr plain $ str $ show $  s^.move

drawGrid :: TuiState -> Widget ResourceName
drawGrid s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Draughts")
  $ vBox [hBox [drawSquare s cursor square | square <- row] | row <- gameBoard]
  where
    cursor = nonEmptyCursorCurrent $ nonEmptyCursorCurrent $ view board s
    ctl = (NE.toList . rebuildNonEmptyCursor)
    gameBoard = map ctl $ ctl $ view board s
-- Drawing logic
{-
Each square is represented by a 3x3 grid.
The bottom square is solid, and only depends even/odd.
The middle square may have a piece (king or piece), which is displayed as a red/black square.
The top is the same, its just whether or not there is a king.
-}
block :: String
block = "   "

threeBlock :: String
threeBlock = block ++ block ++ block

drawOuterBlock :: Coord -> Coord -> Widget ResourceName
drawOuterBlock cursor xy = case (cursor == xy, isEven xy) of
  (True, True) -> withAttr evenSel $ str $ block
  (True, False) -> withAttr oddSel $ str $ block
  (False, True) ->  withAttr  evenBlock $ str $ block
  (False, False) -> withAttr oddBlock $ str $ block

drawCenterBlock :: TuiState -> Coord -> Coord -> Widget ResourceName
drawCenterBlock s b c
  | c `elem` red = withAttr redPiece $ str $ block
  | c `elem` black = withAttr blackPiece $ str $ block
  | otherwise = drawOuterBlock b c
    where
      red = (s^.game^.redPieces) ++ (s^.game^.redKings)
      black = (s^.game^.blackPieces) ++ (s^.game^.blackKings)

drawTopBlock :: TuiState -> Coord -> Coord -> Widget ResourceName
drawTopBlock s b c
  | c `elem` red = withAttr redPiece $ str $ block
  | c `elem` black = withAttr blackPiece $ str $ block
  | otherwise = drawOuterBlock b c
    where
      red = (s^.game^.redKings)
      black = (s^.game^.blackKings) 

drawSquare :: TuiState -> Coord -> Coord -> Widget ResourceName
drawSquare s b c = vBox [ hBox [o,t,o]
                        , hBox [o,m,o]
                        , hBox [o,o,o]
                        ]
                   where
                     o = drawOuterBlock b c
                     m = drawCenterBlock s b c
                     t = drawTopBlock s b c

-- Basic helper function
isEven :: Coord -> Bool
isEven (x,y) = (mod (x+y) 2) == 0

-- Artifact that should be fixed.
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = drawUI ts

{-
Event Handling
-}

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s = case (s^.game^.status) of
  GameOver -> gameOverTuiEvent s
  Red -> case s^.redMove of
           Human -> humanTuiEvent s
           AI f -> cpuTuiEvent $ set move (f (s^.game)) s
  Black -> case s^.blackMove of
             Human -> humanTuiEvent s
             AI f -> cpuTuiEvent $ set move  (f (s^.game)) s 

gameOverTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
gameOverTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey (KChar 'n') [] ->
            continue $ buildInitialState
                         (view redMove s)
                         (view blackMove s)
                         (view moveLogic s)
                         initialGameState
        _ -> continue s
    _ -> continue s

cpuTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
cpuTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KEnter [] ->
          continue $ set move [] $ over game ((s^.moveLogic) (s^.move)) s
        _ -> continue s
    _ -> continue s


humanTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
humanTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KRight [] -> continue $ over board moveRight s
        EvKey KLeft [] -> continue $ over board moveLeft s
        EvKey KDown [] -> case nonEmptyCursorSelectNext (view board s)  of
                            Nothing -> continue s
                            Just s' -> continue $ set board s' s
        EvKey KUp [] -> case nonEmptyCursorSelectPrev (view board s) of
                            Nothing -> continue s
                            Just s' -> continue $ set board s' s
        EvKey KEnter [] -> continue $ resetMove $ moveFun s
          where
            moveFun = over game ((s^.moveLogic) (s^.move))
            resetMove = set move []
        EvKey (KChar ' ') [] -> continue $ over move (\l -> l ++ [x]) s
          where
            x = accessCursor $ view board s
        _ -> continue s
    _ -> continue s


--HelperFunctions for moving
moveRight :: Board -> Board
moveRight b = case (NE.head list) of
                Nothing -> b
                Just _ -> fromJust $
                  makeNonEmptyCursorWithSelection n $ NE.map fromJust list
  where
    list = NE.map nonEmptyCursorSelectNext $ rebuildNonEmptyCursor b
    n = length $ nonEmptyCursorPrev b

moveLeft :: Board -> Board
moveLeft b = case (NE.head list) of
                Nothing -> b
                Just _ -> fromJust $ makeNonEmptyCursorWithSelection n $ NE.map fromJust list
  where
    list = NE.map nonEmptyCursorSelectPrev $ rebuildNonEmptyCursor b
    n = length $ nonEmptyCursorPrev b


{-
Interact with student code
-}



getMove :: GameState -> [SC.Coord]
getMove = getMove'

getMove' :: GameState -> [SC.Coord]
getMove' _ = []

