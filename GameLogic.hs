module GameLogic where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Debug.Trace

import CalcGeom
import Time
import GameState
import Input

gameLogic :: Event t Duration -> Behavior t ArrowKeys -> Behavior t GameState
gameLogic dtE arrowKeysB = GameState <$> pos
  where
    accel = (vmult 500) <$> arrowKeysB

    zero = (0,0) :: Pos
    (_, vel) = integralVSP cols accel dtE
    (_, pos) = integralVSP colp vel dtE

    cols = whenE (isCollision <$> pos) ((collision <$> pos <*> vel) <@ dtE) 
    colp = whenE (isCollision <$> pos) ((collisionP <$> pos) <@ dtE) 
    

isCollision (x,y) = colX || colY
  where
    colX | x > 1920 || x < 0 = True
         | otherwise = False
    colY | y > 1080 || y < 0 = True
         | otherwise = False
collision (x,y) (vx, vy) = (colX, colY)
  where
    colX | x > 1920 || x < 0 = -vx
         | otherwise = vx
    colY | y > 1080 || y < 0 = -vy
         | otherwise = vy

collisionP (x,y) = (colX, colY)
  where
    colX | x > 1920 = 1919
         | x < 0 = 1
         | otherwise = x
    colY | y > 1080 = 1079
         | y < 0 = 1
         | otherwise = y
