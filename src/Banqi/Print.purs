module Banqi.Print where

import Prelude

import Banqi.Action (Action(..))
import Banqi.Board (Board(..), Color(..), Label(..), Piece, Square(..))
import Banqi.Position (Position, toString)
import Data.Array (drop, fold, intercalate, replicate, reverse, take)
import Data.Foldable (surround)
import Data.Maybe (fromMaybe)
import Data.String (toLower, toUpper)

printBoard :: Board -> String
printBoard (Board squares) = surround printSpacer (map printRank (reverse $ chunks 8 squares))

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
  printLabel' label
    # case color of
        Red -> toUpper
        Black -> toLower

printLabel' :: Label -> String
printLabel' = case _ of
  General -> "G"
  Advisor -> "A"
  Elephant -> "E"
  Chariot -> "R"
  Horse -> "H"
  Soldier -> "S"
  Cannon -> "C"

printLabel :: Label -> String
printLabel = case _ of
  General -> "General"
  Advisor -> "Advisor"
  Elephant -> "Elephant"
  Chariot -> "Chariot"
  Horse -> "Horse"
  Soldier -> "Soldier"
  Cannon -> "Cannon"

printColor :: Color -> String
printColor = case _ of
  Red -> "Red"
  Black -> "Black"

printPosition :: Position -> String
printPosition = toLower <<< fromMaybe "unknown" <<< toString

printAction :: Action -> String
printAction = case _ of
  Move from to -> "move " <> printPosition from <> " " <> printPosition to
  Turn pos -> "turn " <> printPosition pos
  Capture from to -> "capture " <> printPosition from <> " " <> printPosition to

-- | Utils
chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []

chunks n xs = pure (take n xs) <> (chunks n $ drop n xs)
