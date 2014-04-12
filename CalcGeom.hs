{-# LANGUAGE NoMonomorphismRestriction, ExistentialQuantification, RankNTypes, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, EmptyDataDecls #-}
module CalcGeom where


import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Time.Clock.POSIX
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Data.IORef
import Data.Time.Clock

import Time

class Vector a where
  vadd :: a -> a -> a
  vmult :: (Num b, Real b) => b -> a -> a
  vzero :: a
  vneg :: a -> a

type Pos = (Double, Double)
instance Vector Pos where
  (x,y) `vadd` (x',y') = (x+x',y+y')
  c `vmult` (x,y) = ((realToFrac c)*x,(realToFrac c)*y)
  vzero = (0,0)
  vneg (x,y) = (-x,-y)


integral :: (Vector a) => a -> Behavior t a -> Event t Duration -> (Event t (), Behavior t a)
integral c b dt = mapAccum c deltaAccumulator
  where
    dtd = realToFrac <$> dt
    deltaAccumulator = addDummy <$> (deltaMult <@> dtd)
    deltaMult = (flip vmult) <$> b
    addDummy x y = ((),x `vadd` y)

