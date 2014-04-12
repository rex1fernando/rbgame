{------------------------------------------------------------------------------
    reactive-banana
 
    Implementation of an "industry strength" game loop with fixed time step
    and variable fps.
 
    See also  http://gafferongames.com/game-physics/fix-your-timestep/
-------------------------------------------------------------------------------}
{-# LANGUAGE NoMonomorphismRestriction, ExistentialQuantification, RankNTypes, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, EmptyDataDecls #-}
module Main where
 
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Time.Clock.POSIX
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Data.IORef
import Data.Time.Clock

import Loop
import Time
import CalcGeom
import GameLogic
import Draw
import Input

 
main = SDL.withInit [SDL.InitEverything] $ do
      screen <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
      gameLoop dt (fromIntegral fps) (gameNetwork screen)
      return ()
   
gameNetwork :: SDL.Surface -> (forall t . Frameworks t => GameNetworkDescription t)
gameNetwork screen physt clock input = do
  -- physics always updates with constant time step
  let dtE = (pure dt) <@ physt

  -- game logic behavior
  let stateB = gameLogic dtE (arrowKeysB input)
  
  return $ (draw screen) <$> stateB

