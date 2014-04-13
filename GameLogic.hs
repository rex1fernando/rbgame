module GameLogic where

import Reactive.Banana
import Reactive.Banana.Frameworks

import CalcGeom
import Time
import GameState
import Input

gameLogic :: Event t Duration -> Behavior t ArrowKeys -> Behavior t GameState
gameLogic dtE arrowKeysB = GameState <$> pos
  where
    accel = (vmult 500) <$> arrowKeysB

    zero = (0,0) :: Pos
    (_, vel) = integral zero accel dtE
    (_, pos) = integral zero vel dtE
  
  
