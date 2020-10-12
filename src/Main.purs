module Main where

import Prelude
import Banqi.Game as Banqi
import Banqi.Print (printBoard, printTurn)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (RWSResult(..), runRWS)
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
-- | - Print the board with a1 at the bottom-left
-- | - Make move and capture take the label of the attacking piece as "from position"
-- | - Proper printTurn implementation
-- | - Add win condition
-- | - Add AI player
-- | -----------------------------------------------------------------------------
main :: Effect Unit
main = do
  initialState <- Banqi.init
  interface <- RL.createConsoleInterface RL.noCompletion
  let
    lineHandler :: Banqi.State -> String -> Effect Unit
    lineHandler currentState input = do
      case input of
        "quit" -> RL.close interface
        _ -> do
          case (Banqi.parse input) of
            Just action -> do
              case runRWS (runExceptT $ Banqi.update action) unit currentState of
                RWSResult _ (Left err) _ -> log err
                RWSResult state (Right _) written -> do
                  for_ written log
                  log $ printBoard state.board <> "\n"
                  let
                    prompt = printTurn state.turn <> " > "
                  RL.setPrompt prompt (length prompt) interface
                  RL.setLineHandler interface $ lineHandler state
            Nothing -> log "Invalid command"
          RL.prompt interface
  RL.setPrompt "RED > " 6 interface
  RL.setLineHandler interface $ lineHandler initialState
  RL.prompt interface
