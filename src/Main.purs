module Main where

import Prelude

import Banqi.Game (Outcome(..), State, init, run)
import Banqi.Player (computerAction, help, parse, playerAction)
import Banqi.Print (printColor)
import Banqi.Rules (Action)
import Control.Monad.RWS (RWSResult(..))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)
import Node.ReadLine as RL

-- | -----------------------------------------------------------------------------
-- | TODO:
-- | -----------------------------------------------------------------------------
-- | - Make move and capture take the label of the attacking piece as "from position"
-- | - Improve Computer player
-- | -----------------------------------------------------------------------------
main :: Effect Unit
main = do
  initialState <- init
  interface <- RL.createConsoleInterface RL.noCompletion
  let
    setPrompt currentState = do
      let
        prompt = printColor currentState.turn <> " > "
      RL.setPrompt prompt (length prompt) interface

    lineHandler currentState input = do
      case input of
        "quit" -> RL.close interface
        "help" -> do
          case run help currentState of
            RWSResult _ (Left err) _ -> log err
            RWSResult _ (Right _) written -> for_ written log
          RL.prompt interface
        "auto" -> do
          computerVsComputer currentState (RL.close interface)
        _ -> do
          case parse input of
            Just action -> do
              playerVsComputer action currentState
                (\state -> RL.setLineHandler interface (lineHandler state) *> RL.prompt interface)
                (RL.close interface)
            Nothing -> log "Invalid command" *> RL.prompt interface
  setPrompt initialState
  RL.setLineHandler interface $ lineHandler initialState
  RL.prompt interface

computerVsComputer :: State -> Effect Unit -> Effect Unit
computerVsComputer currentState end = tailRecM computerTurn currentState *> end
  where
  computerTurn =
    run computerAction >>> case _ of
      RWSResult _ (Left err) _ -> log err *> (pure $ Done unit)
      RWSResult state (Right _) written -> do
        for_ written log
        case state.outcome of
          Continue -> pure $ Loop state
          Winner _ -> pure $ Done unit

playerVsComputer :: Action -> State -> (State -> Effect Unit) -> Effect Unit -> Effect Unit
playerVsComputer action currentState continue end = playerTurn currentState computerTurn
  where
  playerTurn state nextTurn =
    case run (playerAction action) state of
      RWSResult _ (Left err) _ -> log err *> continue state
      RWSResult state' (Right _) written -> do
        for_ written log
        case state.outcome of
          Continue -> nextTurn state'
          Winner _ -> end

  computerTurn state =
    case run computerAction state of
      RWSResult _ (Left err) _ -> log err *> continue state
      RWSResult state' (Right _) written' -> do
        for_ written' log
        case state.outcome of
          Continue -> continue state'
          Winner _ -> end

