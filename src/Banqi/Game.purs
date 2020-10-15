module Banqi.Game where

import Prelude

import Banqi.Board (Board, Color(..), flipColor, inventory, peek, read, setup)
import Banqi.Print (printColor, printLabel, printPosition)
import Banqi.Rules (Action(..), performAction)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.RWS (RWS, RWSResult, get, modify_, runRWS, tell)
import Data.Array (null)
import Data.Either (Either)
import Data.Maybe (maybe)
import Effect (Effect)

type State
  = { board :: Board
    , turn :: Color
    , outcome :: Outcome
    }

data Outcome
  = Continue
  | Winner Color

type Log
  = Array String

type Game
  = ExceptT String (RWS Unit Log State)

type GameResult a
  = RWSResult State (Either String a) (Array String)

init :: Effect State
init = do
  board <- setup
  pure { board, turn: Red, outcome: Continue }

run :: forall a. Game a -> State -> GameResult a
run game state = runRWS (runExceptT game) unit state

update :: Action -> Game Unit
update action = do
  state <- get
  let
    newBoard = performAction state.board action
  if playerLoses (flipColor state.turn) newBoard then do
    modify_ _ { board = newBoard, outcome = Winner state.turn }
  else do
    logAction action
    modify_ _ { board = newBoard, turn = flipColor state.turn, outcome = Continue }

logAction :: Action -> Game Unit
logAction action = do
  state <- get
  case action of
    Move from to -> do
      tell [ (printColor state.turn) <> " moved a " <> (maybe "unknown" (_.label >>> printLabel) (read from state.board))  <> " to " <> printPosition to]
    Turn pos -> do
      tell [ (printColor state.turn) <> " revealed a " <> (maybe "unknown" (\{ color, label } -> printColor color <> " " <> printLabel label) (peek pos state.board))  <> " at " <> printPosition pos ]
    Capture from to -> do
      tell [ (printColor state.turn) <> "'s " <> (maybe "unknown" (_.label >>> printLabel) (read from state.board)) <> " captured a " <> (maybe "unknown" (_.label >>> printLabel) (read to state.board)) <> " at " <> printPosition to ]


playerLoses :: Color -> Board -> Boolean
playerLoses player board = null $ inventory player board

gameOver :: Color -> Board -> Boolean
gameOver player board = playerLoses player board || playerLoses (flipColor player) board
