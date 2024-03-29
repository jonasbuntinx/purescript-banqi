module Banqi.Board where

import Prelude

import Banqi.Action (Action(..))
import Banqi.Position (Position, toIndex, valid)
import Banqi.Utils (shuffle)
import Data.Array (foldl, modifyAt, replicate, singleton, updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over)
import Effect (Effect)

data Label
  = General
  | Advisor
  | Elephant
  | Chariot
  | Horse
  | Soldier
  | Cannon

derive instance eqPieceType :: Eq Label

derive instance ordPieceType :: Ord Label

data Color
  = Red
  | Black

derive instance eqColor :: Eq Color

derive instance ordColor :: Ord Color

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
  generate color =
    map FaceDown $ singleton { color, label: General }
      <> replicate 2 { color, label: Advisor }
      <> replicate 2 { color, label: Elephant }
      <> replicate 2 { color, label: Chariot }
      <> replicate 2 { color, label: Horse }
      <> replicate 5 { color, label: Soldier }
      <> replicate 2 { color, label: Cannon }

at :: Position -> Board -> Maybe Square
at pos (Board squares)
  | valid pos = squares !! toIndex pos
  | otherwise = Nothing

look :: Position -> Board -> Maybe Piece
look pos board = case at pos board of
  Just (FaceUp piece) -> Just piece
  _ -> Nothing

peek :: Position -> Board -> Maybe Piece
peek pos board = case at pos board of
  Just (FaceDown piece) -> Just piece
  _ -> Nothing

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
          square <- at from board
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
inventory player (Board squares) =
  foldl
    ( \acc -> case _ of
        FaceUp { label, color }
          | color == player -> acc <> [ label ]
        FaceDown { label, color }
          | color == player -> acc <> [ label ]
        _ -> acc
    )
    []
    squares

performAction :: Board -> Action -> Board
performAction board = case _ of
  Move from to -> move from to board
  Turn pos -> turn pos board
  Capture from to -> move from to board
