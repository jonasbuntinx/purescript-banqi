module Banqi.Action where

import Prelude

import Banqi.Position (Position)

data Action
  = Move Position Position
  | Turn Position
  | Capture Position Position

derive instance eqAction :: Eq Action

