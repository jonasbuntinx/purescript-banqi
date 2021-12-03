module Banqi.Utils where

import Prelude

import Data.Array (length, sortBy, zip)
import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicateA)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)

maximum' :: Array Number -> Number
maximum' xs = case maximum xs of
  Just m -> m
  Nothing -> 0.0

minimum' :: Array Number -> Number
minimum' xs = case minimum xs of
  Just m -> m
  Nothing -> 0.0

shuffle :: forall a m. MonadEffect m => Array a -> m (Array a)
shuffle xs = do
  ns <- liftEffect $ replicateA (length xs) (randomInt 0 top)
  pure (map snd (sortBy (comparing fst) (zip ns xs)))
