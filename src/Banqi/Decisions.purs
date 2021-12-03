module Banqi.Decisions where

import Prelude

import Banqi.Action (Action(..))
import Banqi.Board (Board, Color, Label(..), flipColor, inventory, performAction)
import Banqi.Game (Game, gameOver)
import Banqi.Rules (possibleActions)
import Banqi.Utils (maximum', minimum', shuffle)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (get)
import Data.Array (filter)
import Data.Array.Partial (head, tail)
import Data.Foldable (maximumBy, sum)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

-- | Heuristic value
type Score
  = Number

type Depth
  = Int

type Alpha
  = Number

type Beta
  = Number

value :: Label -> Score
value = case _ of
  General -> 6.0
  Advisor -> 5.0
  Elephant -> 4.0
  Chariot -> 3.0
  Horse -> 2.0
  Soldier -> 1.0
  Cannon -> 5.0

evaluate :: Color -> Board -> Score
evaluate player board = 50.0 - (sum $ map value $ inventory (flipColor player) board)

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
  | depth == 0 || gameOver player board = pure $ evaluate player board
  | otherwise =
      do
        state <- get
        nextBoards <- do
          opponentActions <- possibleActions board (flipColor player)
          shuffled <- shuffle opponentActions
          pure $ shuffled
            # filter case _ of
                Turn _ -> false
                _ -> true
            # map \action -> performAction board action
        score <- do
          if player == state.turn then
            pure <<< maximum' =<< maxAbMap alpha beta (abMinimax (depth - 1) (flipColor player)) nextBoards
          else
            pure <<< minimum' =<< minAbMap alpha beta (abMinimax (depth - 1) (flipColor player)) nextBoards
        pure score

calculateScore :: Action -> Game (Tuple Action Score)
calculateScore action = do
  state <- get
  score <- case action of
    Turn _ -> pure $ evaluate state.turn state.board
    _ -> abMinimax 3 state.turn (-1000.0) 1000.0 (performAction state.board action)
  pure $ Tuple action score

decideAction :: Game (Maybe Action)
decideAction = do
  state <- get
  actions <- possibleActions state.board state.turn
  shuffled <- shuffle actions
  scores <- traverse calculateScore shuffled
  pure $ fst <$> (maximumBy (\x y -> compare (snd x) (snd y))) scores
