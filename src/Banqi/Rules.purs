module Banqi.Rules where

import Prelude

import Banqi.Board (Board(..), Color, Label(..), Square(..), look, move, read, turn)
import Banqi.Position (Position, down, fromIndex, left, right, up)
import Data.Array (concat, elem, filter, mapMaybe, mapWithIndex)
import Data.Maybe (Maybe(..))

data Action
  = Move Position Position
  | Turn Position
  | Capture Position Position

derive instance eqAction :: Eq Action

isLegal :: Color -> Board -> Action -> Boolean
isLegal color board action = action `elem` (possibleActions board color)

possibleActions :: Board -> Color -> Array Action
possibleActions board@(Board squares) turn =
  concat
    $ flip mapWithIndex squares
    $ fromIndex
    >>> \pos -> case _ of
        FaceDown _ -> [ Turn pos ]
        FaceUp { color, label } | color == turn -> moveActions board pos <>
          case label of
            Cannon -> cannonActions board pos
            _ -> captureActions board pos
        _ -> []

moveActions :: Board -> Position -> Array Action
moveActions board pos =
  apply [ up, down, left, right ] [ pos ]
    # filter canMove
    # map (Move pos)
  where
  canMove pos' = case look pos board, look pos' board of
    Just (FaceUp _), Just Empty -> true
    _, _ -> false

captureActions :: Board -> Position -> Array Action
captureActions board pos = apply [ up, down, left, right ] [ pos ]
  # filter canCapture
  # map (Capture pos)
  where
  canCapture pos' = case read pos board, read pos' board of
    Just { color, label }, Just target | color /= target.color ->
      case label of
          General -> target.label `elem` [ General, Advisor, Elephant, Chariot, Horse, Cannon ]
          Advisor -> target.label `elem` [ Advisor, Elephant, Chariot, Horse, Soldier, Cannon ]
          Elephant -> target.label `elem` [ Elephant, Chariot, Horse, Soldier, Cannon ]
          Chariot -> target.label `elem` [ Chariot, Horse, Soldier, Cannon ]
          Horse -> target.label `elem` [ Horse, Soldier, Cannon ]
          Soldier -> target.label `elem` [ General, Soldier ]
          Cannon -> false
    _, _ -> false

cannonActions :: Board -> Position -> Array Action
cannonActions board pos = mapMaybe findMark [ up, down, left, right ]
  # filter canCapture
  # map (Capture pos)
  where
  canCapture pos' = case read pos board, read pos' board of
    Just { color, label: Cannon }, Just target | color /= target.color -> true
    _, _ -> false

  findMark fn = scout pos false
    where
    scout p screenFound =
      let
        next = fn p
      in
        case look next board of
          Nothing -> Nothing
          Just Empty -> scout next screenFound
          Just _ -> if screenFound then Just next else scout next true

performAction :: Board -> Action -> Board
performAction board = case _ of
  Move from to -> move from to board
  Turn pos -> turn pos board
  Capture from to -> move from to board
