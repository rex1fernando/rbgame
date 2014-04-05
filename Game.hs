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

 
main = SDL.withInit [SDL.InitEverything] $ do
      screen <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
      gameLoop dt (fromIntegral fps) (gameNetwork screen)
      return ()
   
gameNetwork :: SDL.Surface -> (forall t . Frameworks t => GameNetworkDescription t)
gameNetwork screen physt clock input = do
  let x = (*90) <$> clock
  let keys = filterE isKeyEvent input
  let leftDown = stepper False $ keyDownB SDL.SDLK_LEFT keys
  let rightDown = stepper False $ keyDownB SDL.SDLK_RIGHT keys
  let a = (accel' <$> leftDown <*> rightDown) 
  let accel = a <@ physt
  let accelF = (\x -> (+(x*dt))) <$> accel
  let vel = accumB 0 accelF 
  let velF = (\x -> (+(x*dt))) <$> vel
  let pos = accumB 0 (velF <@ physt)

  let upDown = stepper False $ keyDownB SDL.SDLK_UP keys
  let downDown = stepper False $ keyDownB SDL.SDLK_DOWN keys
  let ay = (accel' <$> upDown <*> downDown) 
  let accely = ay <@ physt
  let accelyF = (\x -> (+(x*dt))) <$> accely
  let vely= accumB 0 accelyF
  let velyF = (\x -> (+(x*dt))) <$> vely
  let posy = accumB 0 (velyF <@ physt)
  
  return $ ioA <$> pos <*> posy

  where
    accel' True True = 0
    accel' True False = -70
    accel' False True = 70
    accel' False False = 0

    vel' a v = v+a

    ioA x y = do
      white <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 
      blue <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200

      SDL.fillRect screen Nothing white
      SDL.fillRect screen (Just $ SDL.Rect (round x) (round y) 50 50) blue
      
      aaPolygon screen [(300,300), (320,300),
                        (320,320), (300,320)] blue

      SDL.flip screen
      
      putStrLn $ show y

      return ()

 
 
isKeyEvent (SDL.KeyDown _) = True
isKeyEvent (SDL.KeyUp _) = True
isKeyEvent _ = False

keyDownB k e = isDown <$> filterE (isAboutKey k) e
  where
    isAboutKey k (SDL.KeyUp (SDL.Keysym k' _ _)) | k == k' = True
    isAboutKey k (SDL.KeyDown (SDL.Keysym k' _ _)) | k == k' = True
    isAboutKey k _ = False

    isDown (SDL.KeyDown _) = True
    isDown (SDL.KeyUp _) = False
