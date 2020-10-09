module Banqi.Position where

import Prelude
import Data.Bifunctor (lmap, rmap)
import Data.Int (fromString, quot)
import Data.Maybe (Maybe(..))
import Data.String (singleton, toCodePointArray)
import Data.Tuple (Tuple(..))

type File
  = Int

type Rank
  = Int

type Position
  = Tuple File Rank

toPosition :: String -> Maybe Position
toPosition =
  toCodePointArray >>> map singleton
    >>> case _ of
        [ f, r ] -> do
          f' <- fromString f
          r' <- fromString r
          pure $ (Tuple f' r')
        _ -> Nothing

fromIndex :: Int -> Position
fromIndex n = Tuple (n `mod` 8) (n `quot` 8)

toIndex :: Position -> Int
toIndex (Tuple f r) = 8 * r + f

up :: Position -> Position
up = rmap (_ + 1)

down :: Position -> Position
down = rmap (_ - 1)

left :: Position -> Position
left = lmap (_ - 1)

right :: Position -> Position
right = lmap (_ + 1)
