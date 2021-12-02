module Main where

import Prelude

import Banqi.Action (Action)
import Banqi.Game (Outcome(..), State, init, run)
import Banqi.Player (computerAction, help, parse, playerAction, suggest)
import Banqi.Print (printColor)
import Control.Monad.RWS (RWSResult(..))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.ReadLine as RL

-- | -----------------------------------------------------------------------------
-- | TODO:
-- | -----------------------------------------------------------------------------
-- | - Use labels instead of positions for movement and capturing
-- | - Improve AI
-- | - Detect stalemate
-- | -----------------------------------------------------------------------------
main :: Effect Unit
main = do
  initialState <- init
  interface <- RL.createConsoleInterface RL.noCompletion
  let
    setPrompt currentState = do
      let
        prompt = printColor currentState.turn <> " > "
      RL.setPrompt prompt interface

    lineHandler currentState input = do
      case input of
        "quit" -> RL.close interface
        "help" -> do
          result <- run help currentState
          case result of
            RWSResult _ (Left err) _ -> log err
            RWSResult _ (Right _) written -> for_ written log
          RL.prompt interface
        "suggest" -> do
          result <- run suggest currentState
          case result of
            RWSResult _ (Left err) _ -> log err
            RWSResult _ (Right _) written -> for_ written log
          RL.prompt interface
        "auto" -> do
          computerVsComputer currentState (RL.close interface)
        _ -> do
          case parse input of
            Just action -> do
              playerVsComputer action currentState
                (\state -> RL.setLineHandler (lineHandler state) interface *> RL.prompt interface)
                (RL.close interface)
            Nothing -> log "Invalid command" *> RL.prompt interface
  setPrompt initialState
  RL.setLineHandler (lineHandler initialState) interface
  RL.prompt interface

computerVsComputer :: State -> Effect Unit -> Effect Unit
computerVsComputer currentState end = tailRecM computerTurn currentState *> end
  where
  computerTurn state = do
    result <- run computerAction state
    case result of
      RWSResult _ (Left err) _ -> log err *> (pure $ Done unit)
      RWSResult state' (Right _) written -> do
        for_ written log
        case state'.outcome of
          Continue -> pure $ Loop state'
          Winner _ -> pure $ Done unit

playerVsComputer :: Action -> State -> (State -> Effect Unit) -> Effect Unit -> Effect Unit
playerVsComputer action currentState continue end = playerTurn currentState computerTurn
  where
  playerTurn state nextTurn = do
    result <- run (playerAction action) state
    case result of
      RWSResult _ (Left err) _ -> log err *> continue state
      RWSResult state' (Right _) written -> do
        for_ written log
        case state.outcome of
          Continue -> nextTurn state'
          Winner _ -> end

  computerTurn state = do
    result <- run computerAction state
    case result of
      RWSResult _ (Left err) _ -> log err *> continue state
      RWSResult state' (Right _) written' -> do
        for_ written' log
        case state.outcome of
          Continue -> continue state'
          Winner _ -> end
