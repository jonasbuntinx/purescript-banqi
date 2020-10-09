module Banqi.Rules where

import Prelude
import Banqi.Board (Board, Color, Label(..), Square(..), lookupBoard, mapBoard)
import Banqi.Position (Position, down, left, right, up)
import Data.Array (concat, elem, filter)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data Action
  = Move Position Position
  | Turn Position
  | Capture Position Position

derive instance eqAction :: Eq Action

legal :: Color -> Board -> Action -> Boolean
legal color board action = action `elem` (actions board color)

actions :: Board -> Color -> Array Action
actions board turn =
  concat
    $ flip mapBoard board \pos -> case _ of
        FaceDown piece -> [ Turn pos ]
        FaceUp piece ->
          if piece.color /= turn then
            []
          else
            moveActions board pos <> captureActions board pos -- TODO: Special cannon rules
        Empty -> []

moveActions :: Board -> Position -> Array Action
moveActions board pos@(Tuple f r) =
  apply [ up, down, left, right ] [ pos ]
    # filter canMove
    # map (Move pos)
  where
  canMove to = case lookupBoard board pos, lookupBoard board to of
    Just (FaceUp _), Just Empty -> true
    _, _ -> false

captureActions :: Board -> Position -> Array Action
captureActions board pos =
  apply [ up, down, left, right ] [ pos ]
    # filter canCapture
    # map (Capture pos)
  where
  canCapture to = case lookupBoard board pos, lookupBoard board to of
    Just (FaceUp attacker), Just (FaceUp target) ->
      attacker.color /= target.color
        && case attacker.label of
            General -> target.label `elem` [ General, Advisor, Elephant, Chariot, Horse, Cannon ]
            Advisor -> target.label `elem` [ Advisor, Elephant, Chariot, Horse, Soldier, Cannon ]
            Elephant -> target.label `elem` [ Elephant, Chariot, Horse, Soldier, Cannon ]
            Chariot -> target.label `elem` [ Chariot, Horse, Soldier, Cannon ]
            Horse -> target.label `elem` [ Horse, Soldier, Cannon ]
            Soldier -> target.label `elem` [ General, Soldier ]
            Cannon -> false
    _, _ -> false
