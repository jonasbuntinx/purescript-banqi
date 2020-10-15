module Banqi.Position where

import Prelude

import Control.MonadZero (guard)
import Data.Bifunctor (lmap, rmap)
import Data.Enum (fromEnum, toEnum)
import Data.Int (fromString, quot, radix, toStringAs) as Int
import Data.Maybe (Maybe(..))
import Data.String (codePointAt, singleton, toCodePointArray, toUpper)
import Data.Tuple (Tuple(..))

type File
  = Int

type Rank
  = Int

type Position
  = Tuple File Rank

fromString :: String -> Maybe Position
fromString =
  toCodePointArray >>> map singleton
    >>> case _ of
        [ f, r ] -> do
          f' <- (_ - 65) <$> fromEnum <$> codePointAt 0 (toUpper f)
          r' <- (_ - 1) <$> Int.fromString r
          guard (between 0 4 r' && between 0 7 f')
          pure $ (Tuple f' r')
        _ -> Nothing

toString :: Position -> Maybe String
toString (Tuple f r) =  do
  f' <- singleton <$> toEnum (f + 65)
  rx <- Int.radix 10
  pure (f' <> Int.toStringAs rx (r + 1))

fromIndex :: Int -> Position
fromIndex n = Tuple (n `mod` 8) (n `Int.quot` 8)

toIndex :: Position -> Int
toIndex (Tuple f r) = 8 * r + f

valid :: Position -> Boolean
valid (Tuple f r) = between 0 4 r && between 0 7 f

up :: Position -> Position
up = rmap (_ + 1)

down :: Position -> Position
down = rmap (_ - 1)

left :: Position -> Position
left = lmap (_ - 1)

right :: Position -> Position
right = lmap (_ + 1)
