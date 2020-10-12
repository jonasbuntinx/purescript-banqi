module Banqi.Print where

import Prelude

import Banqi.Board (Board(..), Color(..), Label(..), Piece, Square(..))
import Banqi.Rules (Action(..))
import Data.Array (drop, fold, intercalate, replicate, reverse, take)
import Data.Foldable (surround)
import Data.String (toLower, toUpper)

printBoard :: Board -> String
printBoard (Board squares) =  surround printSpacer (map printRank (reverse $ chunks 8 squares))

printRank :: Array Square -> String
printRank rank = intercalate "" (map printSquare rank)

printSpacer :: String
printSpacer = fold (replicate 16 " ") <> "\n"

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

-- | Utils
chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = pure (take n xs) <> (chunks n $ drop n xs)
