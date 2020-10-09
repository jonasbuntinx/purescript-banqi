module Banqi.Print where

import Prelude
import Banqi.Board (Board(..), Color(..), Label(..), Piece, Square(..))
import Banqi.Rules (Action(..))
import Data.Array (fold, intercalate, mapWithIndex, replicate)
import Data.String (toLower, toUpper)

printBoard :: Board -> String
printBoard (Board squares) =
  intercalate ""
    $ mapWithIndex
        ( \i s -> case i `mod` 8 of
            0 -> printSpacer <> "\n" <> printSquare s
            _ -> printSquare s
        )
        squares

printSpacer :: String
printSpacer = fold (replicate 16 " ")

printSquare :: Square -> String
printSquare = case _ of
  FaceDown _ -> " ?"
  FaceUp p -> " " <> printPiece p
  Empty -> " ."

printPiece :: Piece -> String
printPiece { color, label } =
  printLabel label
    # case color of
        Red -> toUpper
        Black -> toLower

printLabel :: Label -> String
printLabel = case _ of
  General -> "G"
  Advisor -> "A"
  Elephant -> "E"
  Chariot -> "R"
  Horse -> "H"
  Soldier -> "S"
  Cannon -> "C"

printTurn :: Color -> String
printTurn = case _ of
  Red -> "RED"
  Black -> "black"

printUpdate :: Color -> Action -> String
printUpdate turn action =
  case turn of
    Red -> "Red"
    Black -> "Black"
    <> " performed a "
    <> ( case action of
          Turn _ -> "turn"
          Move _ _ -> "move"
          Capture _ _ -> "capture"
      )
