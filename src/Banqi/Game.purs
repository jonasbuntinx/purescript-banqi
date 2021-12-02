module Banqi.Game where

import Prelude

import Banqi.Action (Action(..))
import Banqi.Board (Board, Color(..), flipColor, inventory, look, peek, performAction, setup)
import Banqi.Print (printColor, printLabel, printPosition)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.RWS (RWSResult, RWST, get, modify_, runRWST, tell)
import Data.Array (cons, null, singleton)
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)

type State
  =
  { board :: Board
  , turn :: Color
  , history :: Map Color (Array Action)
  , outcome :: Outcome
  }

data Outcome
  = Continue
  | Winner Color

type Log
  = Array String

type Game
  = ExceptT String (RWST Unit Log State Effect)

type GameResult a
  = RWSResult State (Either String a) (Array String)

init :: Effect State
init = do
  board <- setup
  pure { board, turn: Red, history: Map.empty, outcome: Continue }

run :: forall a. Game a -> State -> Effect (GameResult a)
run game state = runRWST (runExceptT game) unit state

update :: Action -> Game Unit
update action = do
  logAction action
  state <- get
  let
    newBoard = performAction state.board action

    newHistory =
      Map.alter
        ( case _ of
            Nothing -> pure $ singleton action
            Just actions -> pure $ cons action actions
        )
        state.turn
        state.history
  if playerLoses (flipColor state.turn) newBoard then do
    modify_ _ { board = newBoard, history = newHistory, outcome = Winner state.turn }
  else do
    modify_ _ { board = newBoard, turn = flipColor state.turn, history = newHistory, outcome = Continue }

logAction :: Action -> Game Unit
logAction action = do
  state <- get
  case action of
    Move from to -> do
      tell [ (printColor state.turn) <> " moved a " <> (maybe "unknown" (_.label >>> printLabel) (look from state.board)) <> " to " <> printPosition to ]
    Turn pos -> do
      tell [ (printColor state.turn) <> " revealed a " <> (maybe "unknown" (\{ color, label } -> printColor color <> " " <> printLabel label) (peek pos state.board)) <> " at " <> printPosition pos ]
    Capture from to -> do
      tell [ (printColor state.turn) <> "'s " <> (maybe "unknown" (_.label >>> printLabel) (look from state.board)) <> " captured a " <> (maybe "unknown" (_.label >>> printLabel) (look to state.board)) <> " at " <> printPosition to ]

playerLoses :: Color -> Board -> Boolean
playerLoses player board = null $ inventory player board

gameOver :: Color -> Board -> Boolean
gameOver player board = playerLoses player board || playerLoses (flipColor player) board
