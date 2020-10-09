module Banqi.Board where

import Prelude
import Banqi.Position (Position, fromIndex, toIndex)
import Data.Array (length, mapWithIndex, modifyAt, replicate, sortBy, updateAt, zip, (!!))
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

derive instance eqSquare :: Eq Square

newtype Board
  = Board (Array Square)

derive instance newtypeBoard :: Newtype Board _

setupBoard :: Effect Board
setupBoard = Board <$> (shuffle $ squares Red <> squares Black)
  where
  squares color =
    [ FaceDown { color, label: General } ]
      <> replicate 2 (FaceDown { color, label: Advisor })
      <> replicate 2 (FaceDown { color, label: Elephant })
      <> replicate 2 (FaceDown { color, label: Chariot })
      <> replicate 2 (FaceDown { color, label: Horse })
      <> replicate 5 (FaceDown { color, label: Soldier })
      <> replicate 2 (FaceDown { color, label: Cannon })

  shuffle xs = do
    ns <- replicateA (length xs) (randomInt 0 top)
    pure (map snd (sortBy (comparing fst) (zip ns xs)))

mapBoard :: forall a. (Position -> Square -> a) -> Board -> Array a
mapBoard f (Board b) = mapWithIndex (f <<< fromIndex) b

lookupBoard :: Board -> Position -> Maybe Square
lookupBoard (Board b) pos = if i < 0 || i > 31 then Nothing else b !! i
  where
  i = toIndex pos

turnPiece :: Position -> Board -> Board
turnPiece pos =
  over Board
    ( \b ->
        fromMaybe b
          $ modifyAt (toIndex pos)
              ( \s -> case s of
                  FaceDown piece -> FaceUp piece
                  _ -> s
              )
              b
    )

movePiece :: Position -> Position -> Board -> Board
movePiece from to board =
  over Board
    ( \b ->
        fromMaybe b do
          square <- lookupBoard board from
          case square of
            FaceUp _ -> do
              b' <- updateAt (toIndex from) Empty b
              updateAt (toIndex to) square b'
            _ -> Just b
    )
    board
