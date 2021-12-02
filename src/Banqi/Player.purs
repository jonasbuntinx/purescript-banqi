module Banqi.Player where

import Prelude

import Banqi.Action (Action(..))
import Banqi.Board (Color(..))
import Banqi.Decisions (calculateScore, decideAction)
import Banqi.Game (Game, Outcome(..), update)
import Banqi.Position (fromString)
import Banqi.Print (printAction, printBoard)
import Banqi.Rules (isPossible, possibleActions)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

computerAction :: Game Unit
computerAction = do
  decideAction >>=
    case _ of
      Nothing -> throwError "Couldn't decide action"
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
  { board: currentBoard, turn } <- get
  isPossible turn currentBoard action >>=
    if _ then do
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
  { board, turn } <- get
  actions <- possibleActions board turn
  tell $ actions <#> printAction

suggest :: Game Unit
suggest = do
  { board, turn } <- get
  actions <- possibleActions board turn
  scores <- traverse calculateScore actions
  tell $ scores <#> \(Tuple action score) -> printAction action <> ": " <> show score

