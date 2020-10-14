module Banqi.Computer where

import Prelude

import Banqi.Board (flipColor)
import Banqi.Decisions (calculateScores)
import Banqi.Game (Game, Outcome(..), update)
import Banqi.Print (printBoard)
import Banqi.Rules (performAction, possibleActions)
import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import Data.Array (zip)
import Data.Foldable (maximumBy)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)

computerAction :: Game Outcome
computerAction = do
  state <- get
  let
    actions = possibleActions state.board state.turn
    newBoards = map (performAction state.board) actions
  scores <- traverse (calculateScores 5 (flipColor state.turn) (-100) 100) newBoards
  case fst <$> (maximumBy (\x y -> compare (snd x) (snd y))) (zip actions scores) of
    Nothing -> pure $ Winner (flipColor state.turn)
    Just action -> do
      outcome <- update action
      get >>= \{ board } -> tell [ printBoard board ]
      pure outcome


