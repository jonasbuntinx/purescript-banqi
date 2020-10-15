module Banqi.Decisions where

import Prelude

import Banqi.Board (Board(..), Color, Label(..), flipColor, fromSquare)
import Banqi.Game (Game, gameOver)
import Banqi.Rules (Action, performAction, possibleActions)
import Control.Monad.State (get)
import Data.Array (zip)
import Data.Array.Partial (head, tail)
import Data.Foldable (maximum, maximumBy, minimum, sum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Partial.Unsafe (unsafePartial)

-- | Heuristic value
type Score
  = Int

labelScore :: Label -> Score
labelScore = case _ of
  General -> 1
  Advisor -> 1
  Elephant -> 1
  Chariot -> 1
  Horse -> 1
  Soldier -> 1
  Cannon -> 1

evaluate :: Color -> Board -> Score
evaluate player (Board squares) = sum $ map
  ( fromSquare >>> case _ of
    Nothing -> 0
    Just { label, color } -> (if color == player then identity else negate) (labelScore label)
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

abMinimax :: Depth -> Color -> Alpha -> Beta -> Board -> Game Score
abMinimax depth player alpha beta board
  | gameOver player board = pure $ evaluate player board
  | depth == 0 = pure $ evaluate player board
  | otherwise = do
      state <- get
      if player == state.turn
        then pure <<< maximum' =<< maxAbMap alpha beta (abMinimax (depth - 1) (flipColor player)) newBoards
        else pure <<< minimum' =<< minAbMap alpha beta (abMinimax (depth - 1) (flipColor player)) newBoards
  where
  actions = possibleActions board player
  newBoards = map (performAction board) actions

decideAction :: Game (Maybe Action)
decideAction = do
  state <- get
  let
    actions = possibleActions state.board state.turn
    newBoards = map (performAction state.board) actions
  scores <- traverse (abMinimax 0 (flipColor state.turn) (-100) 100) newBoards
  pure $ fst <$> (maximumBy (\x y -> compare (snd x) (snd y))) (zip actions scores)

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
