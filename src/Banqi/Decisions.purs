module Banqi.Decisions where

import Prelude

import Banqi.Board (Board(..), Color, Label(..), flipColor, fromSquare)
import Banqi.Game (Game, gameOver)
import Banqi.Rules (Action(..), performAction, possibleActions)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (get)
import Data.Array (filter)
import Data.Array.Partial (head, tail)
import Data.Foldable (maximum, maximumBy, minimum, sum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

-- | Heuristic value
type Score
  = Int

type Depth
  = Int

type Alpha
  = Int

type Beta
  = Int

labelScore :: Label -> Score
labelScore = case _ of
  General -> 6
  Advisor -> 5
  Elephant -> 4
  Chariot -> 3
  Horse -> 2
  Soldier -> 1
  Cannon -> 7

-- TODO: prevent cat-and-mouse loop
evaluate :: Color -> Board -> Score
evaluate player (Board squares) = sum $ map
  ( fromSquare >>> case _ of
    Nothing -> 0
    Just { label, color } -> weight color $ labelScore label
  ) squares
  where
  weight color = if color == player then identity else negate

maxAbMap :: Alpha -> Beta -> (Alpha -> Beta -> Board -> Game Score) -> Array Board -> Game (Array Score)
maxAbMap alpha' beta fn boards' = tailRecM go { alpha: alpha', boards: boards', acc: [] }
  where
  go { boards: [], acc } = pure $ Done acc
  go { alpha, boards, acc } = do
    score <- unsafePartial $ fn alpha beta (head boards)
    if score >= beta then
      pure $ Done $ acc <> [ beta ]
    else if score > alpha then
      pure $ Loop { alpha: score, boards: rest, acc: acc <> [ score ] }
    else
      pure $ Loop { alpha, boards: rest, acc: acc <> [ score ] }
    where
    rest = unsafePartial $ tail boards

minAbMap :: Alpha -> Beta -> (Alpha -> Beta -> Board -> Game Score) -> Array Board -> Game (Array Score)
minAbMap alpha beta' fn boards' = tailRecM go { beta: beta', boards: boards', acc: [] }
  where
  go { boards: [], acc } = pure $ Done acc
  go { beta, boards, acc } = do
    score <- unsafePartial $ fn alpha beta (head boards)
    if score <= alpha then
      pure $ Done $ acc <> [ alpha ]
    else if score < beta then
      pure $ Loop { beta: score, boards: rest, acc: acc <> [ score ] }
    else
      pure $ Loop { beta, boards: rest, acc: acc <> [ score ] }
    where
    rest = unsafePartial $ tail boards

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
  newBoards = possibleActions board player
    # filter case _ of
        Turn _ -> false
        _ -> true
    # map (performAction board)

calculateScore :: Action -> Game (Tuple Action Score)
calculateScore action = do
  state <- get
  let
    board = performAction state.board action
  score <- case action of
    Turn _ -> pure $ evaluate (flipColor state.turn) board
    _ -> abMinimax 5 (flipColor state.turn) (-100) 100 board
  pure $ Tuple action score

decideAction :: Game (Maybe Action)
decideAction = do
  state <- get
  scores <- traverse calculateScore (possibleActions state.board state.turn)
  pure $ fst <$> (maximumBy (\x y -> compare (snd x) (snd y))) scores

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
