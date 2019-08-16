{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module Checkers where


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

{-
 Datatype Declarations: We begin with the datatype declarations,
 keeping them in a fixed area of the program for easy reference.

 The next task will involve translating these to Lenses, and dealing
 with the changes as the propograte throughout the code.
-}



type Coord = (Int, Int)

type Board = NonEmptyCursor (NonEmptyCursor Coord)

data TuiState =
  TuiState { _board :: Board
           , _move :: [Coord]
           , _game :: GameState}
              deriving (Show, Eq)

data GameState =
  GameState { _blackPieces :: [Coord]
            , _redPieces :: [Coord]
            , _blackKings :: [Coord]
            , _redKings :: [Coord]
            , _blackPlayer :: Player
            , _redPlayer :: Player
            , _status :: Colour
            , _message :: String}
              deriving (Show, Eq)

type PieceFunction = Coord -> String

data Colour = Red | Black | GameOver | NewGame
              deriving (Show, Eq)


data Player = Human | CPU
              deriving (Show, Eq)

type ResourceName = String

{-
  Set up lenses
-}
makeLenses ''TuiState
makeLenses ''GameState
{-
  Cursor functions
-}

accessCursor :: Board -> Coord
accessCursor = nonEmptyCursorCurrent . nonEmptyCursorCurrent

{-
  Main function
-}

tui :: IO ()
tui = do
  let initialState = buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

{-
  Basic tuiApp information. Must be lensified.
-}
  
tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const theAttributes
        }

theAttributes :: AttrMap
theAttributes = attrMap V.defAttr
  [(evenBlock, V.black `on` V.white)
  ,(oddBlock, V.black `on` V.blue)
  ,(evenSel,  V.black `on` V.brightWhite)
  ,(oddSel, V.black `on` V.brightCyan)
  ,(plain, V.black `on` V.white)]

evenBlock, oddBlock, evenSel, oddSel :: AttrName
evenBlock = "evenBlock"
oddBlock = "oddBlock"
evenSel = "evenSel"
oddSel = "oddSel"

plain :: AttrName
plain = "plain"

--initialization        
initializeState :: Player -> Player -> Colour -> TuiState
initializeState a b c= TuiState { _board = initialBoard
                              , _move = []
                              , _game = initializeGameState a b c}


buildInitialState :: TuiState
buildInitialState = TuiState { _board = initialBoard
                             , _move = []
                             , _game = buildInitialGameState}

buildInitialGameState :: GameState
buildInitialGameState = initializeGameState Human Human NewGame


initializeGameState :: Player -> Player -> Colour -> GameState                       
initializeGameState a b c = setMessage $
  GameState { _blackPieces = blackInit
            , _redPieces = redInit
            , _blackKings = []
            , _redKings = []
            , _blackPlayer = a
            , _redPlayer = b
            , _status = c
            , _message = "This is still a work in progress"}


blackInit :: [Coord]
blackInit = [ (1,0), (3,0), (5,0), (7,0)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,2), (3,2), (5,2), (7,2)]

redInit :: [Coord]
redInit = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (0,5), (2,5), (4,5), (6,5)]

initialBoard :: NonEmptyCursor (NonEmptyCursor Coord)
initialBoard = makeNonEmptyCursor $ NE.fromList
  [ makeNonEmptyCursor $
    NE.fromList [(i,j) | i <- [0..7] ] | j <- [0..7]]


{-
  Draw function.
-}

drawUI :: TuiState -> [Widget ResourceName]
drawUI s = case s^.game^.status of
  NewGame -> drawNewGameUI s
  GameOver -> drawGameOverUI s
  _ -> drawGameUI s
               
drawGameUI :: TuiState -> [Widget ResourceName]
drawGameUI s = [ C.center $ padRight (Pad 2) (drawStats s) <+> drawGrid s ]

-- New Game UI

drawNewGameUI :: TuiState -> [Widget ResourceName]
drawNewGameUI _ = [ withBorderStyle BS.unicodeBold
                  $ B.borderWithLabel (str "Game Mode")
                  $ vBox [ withAttr plain $ str $
                          (show (2*(fst x) + (fst y)))
                          ++ ". "
                          ++ (snd x) ++ " vs. " ++ (snd y)
                         | x <- [(0,"Human"), (1,"CPU")]
                         , y <- [(0,"Human"), (1,"CPU")]]]

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
           , moveRow
           ]
    msgRow = withAttr plain $ str $ s^.game^.message
    statusRow = withAttr plain $ str $ show $ s^.game^.status
    moveRow = withAttr plain $ str $ show $ s^.move

drawGrid :: TuiState -> Widget ResourceName
drawGrid s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Draughts")
  $ vBox rows
  where
    gameboard = view board s
    pf = makePf $ view game s
    rows = 
      [ vBox $ reverse $ map (drawUnselectedRow pf) $ nonEmptyCursorPrev gameboard
      , drawSelectedRow pf $ nonEmptyCursorCurrent gameboard
      , vBox $ map (drawUnselectedRow pf) $ nonEmptyCursorNext gameboard ]

makePf :: GameState -> PieceFunction      
makePf gs x
  | x `elem` (view blackPieces gs) = " x "
  | x `elem` (view redPieces gs) = " o "
  | x `elem` (view blackKings gs) = " X "
  | x `elem` (view redKings gs) = " O "
  | otherwise = "   "


drawUnselectedRow :: PieceFunction -> NonEmptyCursor Coord -> Widget ResourceName
drawUnselectedRow pf = hBox . map (getBlock pf False) . toList . rebuildNonEmptyCursor


getBlock :: PieceFunction -> Bool -> Coord -> Widget ResourceName
getBlock pf selected xy =
  let symb = pf xy in
    case (isEven xy, selected) of
      (True, True) -> withAttr evenSel $ str $ symb
      (True, False) -> withAttr evenBlock $ str $ symb
      (False, True) -> withAttr oddSel $ str $ symb
      (False, False) -> withAttr oddBlock $ str $ symb


getSymbol :: [Coord] -> [Coord] -> Coord -> String
getSymbol r b xy 
 | xy `elem` r = " X "
 | xy `elem` b = " O "
 | otherwise = "   "
  

drawSelectedRow :: PieceFunction -> NonEmptyCursor Coord -> Widget ResourceName
drawSelectedRow pf row = hBox (l ++ [c] ++ r')
  where
   l = reverse $ map (getBlock pf False) $ nonEmptyCursorPrev row
   c = getBlock pf True $ nonEmptyCursorCurrent row
   r' = map (getBlock pf False) $ nonEmptyCursorNext row

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
  Red -> case s^.game^.redPlayer of
    Human -> humanTuiEvent s
    CPU -> cpuTuiEvent s
  Black -> case s^.game^.blackPlayer of
    Human -> humanTuiEvent s
    CPU -> cpuTuiEvent s
  GameOver -> humanTuiEvent s
  NewGame -> newTuiEvent s

gameOverTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
gameOverTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s

cpuTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
cpuTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KEnter [] -> continue s'
          where
            m = getMove $ s^.game
            s' = over game (applyMove m) s
        _ -> continue s
    _ -> continue s

newTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
newTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey (KChar '0') [] -> continue $ initializeState Human Human Red
        EvKey (KChar '1') [] -> continue $ initializeState Human CPU Red
        EvKey (KChar '2') [] -> continue $ initializeState CPU Human Red
        EvKey (KChar '3') [] -> continue $ initializeState CPU CPU Red
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
        EvKey KEnter [] -> continue $ over game (applyMove []) (set move [] s)
        EvKey (KChar ' ') [] -> continue $ (%~) move ((:) x) s
          where
            x = accessCursor $ view board s
        _ -> continue s
    _ -> continue s    


--HelperFunctions for moving
moveRight :: Board -> Board
moveRight b = case (NE.head list) of
                Nothing -> b
                Just x -> fromJust $ makeNonEmptyCursorWithSelection n $ NE.map fromJust list
  where
    list = NE.map nonEmptyCursorSelectNext $ rebuildNonEmptyCursor b
    n = length $ nonEmptyCursorPrev b

moveLeft :: Board -> Board
moveLeft b = case (NE.head list) of
                Nothing -> b
                Just x -> fromJust $ makeNonEmptyCursorWithSelection n $ NE.map fromJust list
  where
    list = NE.map nonEmptyCursorSelectPrev $ rebuildNonEmptyCursor b
    n = length $ nonEmptyCursorPrev b

setMessage :: GameState -> GameState
setMessage s = case (s^.status, s^.redPlayer, s^.blackPlayer) of
  (Red, Human, _) -> set message "Human turn, make your move." s
  (Red, CPU, _) -> set message "Press Enter for the CPU to make its move." s
  (Black, _, Human) ->  set message "Human turn, make your move." s
  (Black, _, CPU) -> set message "Press Enter for the CPU to make its move." s
  _ -> s
{-
  Student Functions
-}
applyMove :: [Coord] -> GameState -> GameState
applyMove = applyMove'

applyMove' :: [Coord] -> GameState -> GameState
applyMove' _  s = case s^.status of
  Red -> setMessage $ set status Black s
  Black -> setMessage $ set status Red s
  _ -> buildInitialGameState
               

getMove :: GameState -> [Coord]
getMove = getMove'

getMove' :: GameState -> [Coord]
getMove' _ = []

