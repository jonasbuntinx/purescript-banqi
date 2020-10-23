module Banqi.Player where

import Prelude
import Banqi.Board (Color(..))
import Banqi.Decisions (decideAction)
import Banqi.Game (Game, Outcome(..), update)
import Banqi.Position (fromString)
import Banqi.Print (printBoard, printPosition)
import Banqi.Rules (Action(..), isLegal, possibleActions)
import Control.Monad.Error.Class (throwError)
import Control.Monad.RWS (get, tell)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)

computerAction :: Game Unit
computerAction = do
  state <- get
  maybeAction <- decideAction
  case maybeAction of
    Nothing -> pure unit
    Just action -> do
      update action
      get
        >>= \{ board, outcome } -> do
            tell [ printBoard board ]
            case outcome of
              Continue -> pure unit
              Winner Red -> tell [ "Red won" ]
              Winner Black -> tell [ "Black won" ]

playerAction :: Action -> Game Unit
playerAction action = do
  state <- get
  if isLegal state.turn state.board action then do
    update action
    get
      >>= \{ board, outcome } -> do
          tell [ printBoard board ]
          case outcome of
            Continue -> pure unit
            Winner Red -> tell [ "Red won" ]
            Winner Black -> tell [ "Black won" ]
  else
    throwError "Illegal move"

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

help :: Game Unit
help = do
  state <- get
  tell $ possibleActions state.board state.turn
    <#> case _ of
        Move from to -> "move " <> printPosition from <> " " <> printPosition to
        Turn pos -> "turn " <> printPosition pos
        Capture from to -> "capture " <> printPosition from <> " " <> printPosition to
