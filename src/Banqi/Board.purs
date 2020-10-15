module Banqi.Board where

import Prelude

import Banqi.Position (Position, toIndex, valid)
import Data.Array (foldl, length, modifyAt, replicate, singleton, sortBy, updateAt, zip, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over)
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (randomInt)

data Label
  = General
  | Advisor
  | Elephant
  | Chariot
  | Horse
  | Soldier
  | Cannon

derive instance eqPieceType :: Eq Label

data Color
  = Red
  | Black

derive instance eqColor :: Eq Color

type Piece
  = { color :: Color, label :: Label }

data Square
  = FaceDown Piece
  | FaceUp Piece
  | Empty

newtype Board
  = Board (Array Square)

derive instance newtypeBoard :: Newtype Board _

setup :: Effect Board
setup = map Board (shuffle (generate Red <> generate Black))
  where
  shuffle xs = do
    ns <- replicateA (length xs) (randomInt 0 top)
    pure (map snd (sortBy (comparing fst) (zip ns xs)))

  generate color = map FaceDown $ singleton { color, label: General }
    <> replicate 2 { color, label: Advisor }
    <> replicate 2 { color, label: Elephant }
    <> replicate 2 { color, label: Chariot }
    <> replicate 2 { color, label: Horse }
    <> replicate 5 { color, label: Soldier }
    <> replicate 2 { color, label: Cannon }

look :: Position -> Board -> Maybe Square
look pos (Board squares)
  | valid pos = squares !! toIndex pos
  | otherwise = Nothing

read :: Position -> Board -> Maybe Piece
read pos board = case look pos board of
  Just (FaceUp piece) -> Just piece
  _ -> Nothing

peek :: Position -> Board -> Maybe Piece
peek pos board = case look pos board of
  Just (FaceDown piece) -> Just piece
  _ -> Nothing

fromSquare :: Square -> Maybe Piece
fromSquare = case _ of
  Empty -> Nothing
  FaceUp piece -> Just piece
  FaceDown piece -> Just piece

turn :: Position -> Board -> Board
turn pos =
  over Board
    ( \squares ->
        fromMaybe squares
          $ modifyAt (toIndex pos)
              ( \square -> case square of
                  FaceDown piece -> FaceUp piece
                  _ -> square
              )
              squares
    )

move :: Position -> Position -> Board -> Board
move from to board =
  over Board
    ( \squares ->
        fromMaybe squares do
          square <- look from board
          case square of
            FaceUp _ -> do
              squares' <- updateAt (toIndex from) Empty squares
              updateAt (toIndex to) square squares'
            _ -> Just squares
    )
    board

flipColor :: Color -> Color
flipColor = case _ of
  Red -> Black
  Black -> Red

inventory :: Color -> Board -> Array Label
inventory player (Board squares) = foldl (\acc ->
   case _ of
    FaceUp { label, color } | color == player -> acc <> [ label ]
    FaceDown { label, color } | color == player -> acc <> [ label ]
    _ -> acc) [] squares
