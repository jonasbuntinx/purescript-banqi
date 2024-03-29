module Banqi.Rules where

import Prelude

import Banqi.Action (Action(..))
import Banqi.Board (Board(..), Color, Label(..), Square(..), at, look)
import Banqi.Game (Game)
import Banqi.Position (Position, down, fromIndex, left, right, up)
import Control.Monad.State (get)
import Data.Array (concat, elem, filter, mapMaybe, mapWithIndex, take, uncons)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)

isPossible :: Color -> Board -> Action -> Game Boolean
isPossible color board action = do
  actions <- possibleActions board color
  pure $ action `elem` actions

possibleActions :: Board -> Color -> Game (Array Action)
possibleActions board@(Board squares) turn = do
  pure $ concat $ flip mapWithIndex squares $ fromIndex
    >>> \pos -> case _ of
      FaceDown _ -> [ Turn pos ]
      FaceUp { color, label }
        | color == turn ->
            ( case label of
                Cannon -> cannonActions board pos
                _ -> captureActions board pos
            )
              <> moveActions board pos
      _ -> []

getRecentActions :: Int -> Game (Array Action)
getRecentActions n = do
  state <- get
  Map.lookup state.turn state.history
    # fromMaybe []
    # take n
    # pure

-- This only finds 1 type of pattern: the back-and-forth movement of pieces
-- We need an algorithm then can detect complex patterns
detectStalemate :: Array Action -> Action -> Boolean
detectStalemate [] _ = false
detectStalemate recent (Move from to) = case uncons recent of
  Just { head: (Move from' to'), tail: [] } -> from == to' && to == from'
  Just { head: (Move from' to'), tail } -> from == to' && to == from' && detectStalemate tail (Move from' to')
  _ -> false
detectStalemate _ _ = false

moveActions :: Board -> Position -> Array Action
moveActions board pos =
  apply [ up, down, left, right ] [ pos ]
    # filter (canMove board pos)
    # map (Move pos)

canMove :: Board -> Position -> Position -> Boolean
canMove board pos pos' = case at pos board, at pos' board of
  Just (FaceUp _), Just Empty -> true
  _, _ -> false

captureActions :: Board -> Position -> Array Action
captureActions board pos =
  apply [ up, down, left, right ] [ pos ]
    # filter (canCapture board pos)
    # map (Capture pos)

canCapture :: Board -> Position -> Position -> Boolean
canCapture board pos pos' = case look pos board, look pos' board of
  Just { color, label }, Just target
    | color /= target.color -> case label of
        General -> target.label `elem` [ General, Advisor, Elephant, Chariot, Horse, Cannon ]
        Advisor -> target.label `elem` [ Advisor, Elephant, Chariot, Horse, Soldier, Cannon ]
        Elephant -> target.label `elem` [ Elephant, Chariot, Horse, Soldier, Cannon ]
        Chariot -> target.label `elem` [ Chariot, Horse, Soldier, Cannon ]
        Horse -> target.label `elem` [ Horse, Soldier, Cannon ]
        Soldier -> target.label `elem` [ General, Soldier ]
        Cannon -> true
  _, _ -> false

cannonActions :: Board -> Position -> Array Action
cannonActions board pos =
  mapMaybe (findTarget board pos) [ up, down, left, right ]
    # filter (canCapture board pos)
    # map (Capture pos)

findTarget :: Board -> Position -> (Position -> Position) -> Maybe (Position)
findTarget board pos fn = search pos false
  where
  search p screenFound =
    let
      next = fn p
    in
      case at next board of
        Nothing -> Nothing
        Just Empty -> search next screenFound
        Just _ -> if screenFound then Just next else search next true

