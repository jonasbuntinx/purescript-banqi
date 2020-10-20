module Banqi.Utils where

import Prelude

import Data.Array (length, sortBy, zip)
import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (randomInt)

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

shuffle :: forall a. Array a -> Effect (Array a)
shuffle xs = do
    ns <- replicateA (length xs) (randomInt 0 top)
    pure (map snd (sortBy (comparing fst) (zip ns xs)))
