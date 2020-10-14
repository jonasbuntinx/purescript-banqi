module Main where

import Prelude

import Banqi.Board (Color(..))
import Banqi.Computer (computerAction)
import Banqi.Game (Outcome(..), State, init, run)
import Banqi.Player (help, parse, playerAction)
import Banqi.Print (printColor)
import Banqi.Rules (Action)
import Control.Monad.RWS (RWSResult(..))
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
          computerVsComputer
            (\state -> lineHandler state input)
            (RL.close interface)
            currentState
        _ -> do
          case parse input of
            Just action -> playerVsComputer action
              (\state -> RL.setLineHandler interface (lineHandler state) *> RL.prompt interface)
              (RL.close interface)
              currentState
            Nothing -> log "Invalid command"
  setPrompt initialState
  RL.setLineHandler interface $ lineHandler initialState
  RL.prompt interface

computerVsComputer :: (State -> Effect Unit) -> Effect Unit -> State -> Effect Unit
computerVsComputer continue end state = computerTurn (computerTurn continue end) end state

playerVsComputer :: Action -> (State -> Effect Unit) -> Effect Unit -> State -> Effect Unit
playerVsComputer action continue end state = playerTurn action (computerTurn continue end) end state

playerTurn :: Action -> (State -> Effect Unit) -> Effect Unit -> State  -> Effect Unit
playerTurn action continue end currentState =
  case run (playerAction action) currentState of
    RWSResult _ (Left err) _ -> log err
    RWSResult state (Right outcome) written -> do
      for_ written log
      case outcome of
        Continue -> continue state
        Winner Red -> log "Red won" *> end
        Winner Black -> log "Black won" *> end


computerTurn :: (State -> Effect Unit) -> Effect Unit -> State -> Effect Unit
computerTurn continue end currentState =
  case run computerAction currentState of
    RWSResult _ (Left err) _ -> log err
    RWSResult state (Right outcome) written -> do
      for_ written log
      case outcome of
        Continue -> continue state
        Winner Red -> log "Red won" *> end
        Winner Black -> log "Black won" *>  end
