module Banqi.Decisions where

import Prelude

import Banqi.Board (Board(..), Color, Label(..), Square(..), flipColor)
import Banqi.Game (Game, gameOver)
import Banqi.Rules (performAction, possibleActions)
import Control.Monad.State (get)
import Data.Array.Partial (head, tail)
import Data.Foldable (maximum, minimum, sum)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

-- | Heuristic value
type Score
  = Int

labelScore :: Label -> Score
labelScore = case _ of
  General -> 6
  Advisor -> 4
  Elephant -> 3
  Chariot -> 2
  Horse -> 1
  Soldier -> 5
  Cannon -> 7

evaluate :: Color -> Board -> Score
evaluate player (Board squares) = sum $ map
  ( case _ of
    FaceUp { label, color } -> (if color == player then identity else negate) (labelScore label)
    _ -> 0
  ) squares

-- | Alpha Beta Minimax algorithm
type Depth
  = Int

type Alpha
  = Int

type Beta
  = Int

maxAbMap :: Alpha -> Beta -> (Alpha -> Beta -> Board -> Game Score) -> Array Board -> Game (Array Score)
maxAbMap alpha beta fn [] = pure []
maxAbMap alpha beta fn board = do
  score <- unsafePartial $ fn alpha beta (head board)
  if score >= beta then
    pure [ beta ]
  else if score > alpha then do
    pure <<< append [ score ] =<< maxAbMap score beta fn rest
  else
    pure <<< append [ score ] =<< maxAbMap alpha beta fn rest
  where
    rest = unsafePartial $ tail board

minAbMap :: Alpha -> Beta -> (Alpha -> Beta -> Board -> Game Score) -> Array Board -> Game (Array Score)
minAbMap alpha beta fn [] = pure []
minAbMap alpha beta fn board = do
  score <- unsafePartial $ fn alpha beta (head board)
  if score <= alpha then
    pure [ alpha ]
  else if score < beta then
    pure <<< append [ score ] =<< minAbMap alpha score fn rest
  else
    pure <<< append [ score ] =<< minAbMap alpha beta fn rest
  where
    rest = unsafePartial $ tail  board

calculateScores :: Depth -> Color -> Alpha -> Beta -> Board -> Game Score
calculateScores depth player alpha beta board
  | gameOver player board = pure $ evaluate player board
  | depth == 0 = pure $ evaluate player board
  | otherwise = do
      state <- get
      if player == state.turn
        then pure <<< maximum' =<< maxAbMap alpha beta (calculateScores (depth - 1) (flipColor player)) newBoards
        else pure <<< minimum' =<< minAbMap alpha beta (calculateScores (depth - 1) (flipColor player)) newBoards
  where
  actions = possibleActions board player
  newBoards = map (performAction board) actions

-- | Utils
maximum' :: Array Int -> Int
maximum' xs =
  case maximum xs of
    Just m -> m
    Nothing -> 0

minimum' :: Array Int -> Int
minimum' xs =
  case minimum xs of
    Just m -> m
    Nothing -> 0
