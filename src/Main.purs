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
          computerVsComputer currentState $ RL.close interface
        _ -> do
          case parse input of
            Just action -> do
              state <- playerVsComputer action currentState
              case state.outcome of
                Continue -> RL.setLineHandler interface (lineHandler state) *> RL.prompt interface
                _ -> RL.close interface
            Nothing -> log "Invalid command" *> RL.prompt interface
  setPrompt initialState
  RL.setLineHandler interface $ lineHandler initialState
  RL.prompt interface

computerVsComputer :: State -> Effect Unit -> Effect Unit
computerVsComputer currentState end = tailRecM go currentState *> end
  where
  go =
    run computerAction >>> case _ of
      RWSResult _ (Left err) _ -> log err *> (pure $ Done unit)
      RWSResult state (Right _) written -> do
        for_ written log
        case state.outcome of
          Continue -> pure $ Loop state
          Winner _ -> pure $ Done unit

playerVsComputer :: Action -> State -> Effect State
playerVsComputer action state = do
  state' <- playerTurn action state
  case state'.outcome of
    Continue -> computerTurn state'
    _ -> pure state

playerTurn :: Action -> State -> Effect State
playerTurn action currentState =
  case run (playerAction action) currentState of
    RWSResult state (Left err) _ -> log err *> (pure state)
    RWSResult state (Right _) written -> do
      for_ written log
      pure state

computerTurn :: State -> Effect State
computerTurn currentState =
  case run computerAction currentState of
    RWSResult state (Left err) _ -> log err *> (pure state)
    RWSResult state (Right _) written -> do
      for_ written log
      pure state
