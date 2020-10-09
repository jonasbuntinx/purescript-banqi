module Banqi.Game where

import Prelude
import Banqi.Board (Board, Color(..), move, setup, turn)
import Banqi.Position (fromString)
import Banqi.Print (printUpdate)
import Banqi.Rules (Action(..), isLegal)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.RWS (RWS, get, modify_, tell)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)

type State
  = { board :: Board
    , turn :: Color
    }

type Log
  = Array String

type Game
  = ExceptT String (RWS Unit Log State)

init :: Effect State
init = do
  board <- setup
  pure { board, turn: Red }

update :: Action -> Game Unit
update action = do
  state <- get
  if isLegal state.turn state.board action then do
    case action of
      Move from to -> modify_ $ _ { board = move from to state.board }
      Turn pos -> modify_ $ _ { board = turn pos state.board }
      Capture from to -> modify_ $ _ { board = move from to state.board }
    tell [ printUpdate state.turn action ]
    modify_ $ _ { turn = switchTurn state.turn }
  else
    throwError "Illegal move"

switchTurn :: Color -> Color
switchTurn = case _ of
  Red -> Black
  Black -> Red

parse :: String -> Maybe Action
parse =
  split (Pattern " ")
    >>> case _ of
        [ "turn", pos ] -> fromString pos >>= Turn >>> pure
        [ "move", from, to ] -> do
          from' <- fromString from
          to' <- fromString to
          pure $ Move from' to'
        [ "capture", from, to ] -> do
          from' <- fromString from
          to' <- fromString to
          pure $ Capture from' to'
        _ -> Nothing
