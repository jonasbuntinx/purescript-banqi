module Banqi.Decisions where

import Prelude

import Banqi.Board (Board(..), Color, Label(..), Piece, Square(..), flipColor)
import Banqi.Game (Game, gameOver)
import Banqi.Position (Position, down, fromIndex, left, right, up)
import Banqi.Rules (Action(..), canCapture, canMove, findMark, performAction, possibleActions)
import Banqi.Utils (maximum', minimum')
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (get)
import Data.Array (filter, group, mapMaybe, mapWithIndex, sort)
import Data.Array.NonEmpty as NES
import Data.Array.Partial (head, tail)
import Data.Foldable (maximumBy, sum)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
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

pieceScore :: Board -> Piece -> Score
pieceScore board@(Board squares) piece =
  let
    remaining = fromFoldable $ map (\l -> Tuple (NES.head l) (NES.length l))
      $ group
      $ sort
      $ squares
      # mapMaybe case _ of
        FaceDown p -> Just p
        FaceUp p -> Just p
        Empty -> Nothing

    count p = fromMaybe 0 $ lookup p remaining
  in
    case piece of
      { color, label: General } -> 2
      { color, label: Advisor } -> 2 - count { color, label: Advisor }
      { color, label: Elephant } -> 2 - count { color, label: Elephant }
      { color, label: Chariot } -> 2 - count { color, label: Chariot }
      { color, label: Horse } -> 2 - count { color, label: Horse }
      { color, label: Soldier } ->
        if count { color: flipColor color, label: General } /= 0
          then 5 - count { color, label: Soldier }
          else 1
      { color, label: Cannon } -> 1

captureScore :: Board -> Position -> Score
captureScore board pos = sum $ apply [ up, down, left, right ] [ pos ]
  # filter (canCapture board pos)
  # map (const 1)

cannonCaptureScore :: Board -> Position -> Score
cannonCaptureScore board pos = sum $ mapMaybe (findMark board pos) [ up, down, left, right ]
  # filter (canCapture board pos)
  # map (const 1)

movementScore :: Board -> Position -> Int
movementScore board pos = sum $ apply [ up, down, left, right ] [ pos ]
  # filter (canMove board pos)
  # map (const 1)

positionScore :: Board -> Position -> Int
positionScore board pos = sum [captureScore board pos, movementScore board pos]

cannonScore :: Board -> Position -> Int
cannonScore board pos = sum [ cannonCaptureScore board pos, movementScore board pos]

-- TODO: prevent cat-and-mouse loop
evaluate :: Color -> Board -> Score
evaluate player board@(Board squares) = sum
  $ flip mapWithIndex squares
  $ fromIndex >>> \pos -> case _ of
    Empty -> 0
    FaceDown piece@{ color } -> weight color (pieceScore board piece)
    FaceUp piece@{ color, label } | label == Cannon -> weight color (sum [ pieceScore board piece, cannonScore board pos])
    FaceUp piece@{ color } -> weight color (sum [ pieceScore board piece, positionScore board pos ])
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
    _ -> abMinimax 1 (flipColor state.turn) (-1000) 1000 board
  pure $ Tuple action score

decideAction :: Game (Maybe Action)
decideAction = do
  state <- get
  scores <- traverse calculateScore (possibleActions state.board state.turn)
  pure $ fst <$> (maximumBy (\x y -> compare (snd x) (snd y))) scores
